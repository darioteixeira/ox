open Prelude

include Learner_intf

module Make (Sensors_def : Sensors.DEF) (Action : Action.S) (Maketbl : Dict.MAKETBL) : S with type sensors = Sensors_def.sensors and type action = Action.t =
struct
  module Condition = Condition.Make (Sensors_def)
  module Classifier = Classifier.Make (Condition) (Action)
  module Identifier_dict = Dict.Make (Maketbl (Identifier))
  module Action_map = Map.Make (Action)
  module Action_set = Set.Make (Action)

  type sensors = Sensors_def.sensors

  type action = Action.t

  type environment = sensors Environment.t

  let environment_to_yojson environment =
    let rec loop : type a. a Sensors.t -> a Environment.t -> Yojson.Safe.t list =
      fun sensors environment ->
        match sensors, environment with
        | Sensors.[], Environment.[] ->
            []
        | Sensors.(hd_sensor :: tl_sensors), Environment.(hd_environment :: tl_environment) ->
            let (module Sensor) = hd_sensor in
            let hd = `List Array.(map ~f:Sensor.to_yojson hd_environment |> to_list) in
            let tl = loop tl_sensors tl_environment in
            hd :: tl
    in
    `List (loop Sensors_def.sensors environment)

  let environment_of_yojson = function
    | `List jsons ->
        let rec loop : type a. a Sensors.t -> Yojson.Safe.t list -> (a Environment.t, string) result =
          fun sensors jsons ->
            match sensors, jsons with
            | Sensors.[], [] ->
                Ok Environment.[]
            | Sensors.(hd_sensor :: tl_sensors), (`List hd_json :: tl_jsons) ->
                let (module Sensor) = hd_sensor in
                let (let*) = Result.bind in
                let* hd = Result.map_list ~f:Sensor.of_yojson hd_json |> Result.map Array.of_list in
                let* tl = loop tl_sensors tl_jsons in
                Ok Environment.(hd :: tl)
            | _ ->
                Error "Learner.environment"
        in
        loop Sensors_def.sensors jsons
    | _ ->
        Error "Learner.environment"

  type population = {
    set : Classifier.t Identifier_dict.t;
    numerosity : int; (* Note that numerosity may be different from set cardinality *)
  } [@@deriving yojson]

  type previous = {
    previous_environment: environment;
    previous_action_set : Classifier.t Identifier_dict.t;
    previous_reward : float;
    previous_do_exploration : bool;
  } [@@deriving yojson]

  type best_action_with_prediction = (Action.t * float) Lazy.t

  let best_action_with_prediction_to_yojson v =
    [%derive.to_yojson: Action.t * float] (Lazy.force v)

  let best_action_with_prediction_of_yojson yojson =
    [%derive.of_yojson: Action.t * float] yojson
    |> Result.map Lazy.from_val

  type environment_payload = {
    environment: environment;
    action_set : Classifier.t Identifier_dict.t;
    do_exploration : bool;
    best_action_with_prediction : best_action_with_prediction;
  } [@@deriving yojson]

  type state =
    | Awaiting_environment
    | After_environment of environment_payload
    | After_intermediate_feedback of environment_payload
    [@@deriving yojson]

  type t = {
    mutable config : Config.t;
    mutable current_time : int;
    mutable population : population;
    mutable previous : previous option;
    mutable state : state;
  } [@@deriving yojson]

  type stats = {
    population_size : int;
    population_numerosity : int;
  }

  exception Wrong_state

  (************************************************************************************************)
  (* Logging and formatting.                                                                      *)
  (************************************************************************************************)

  let logs_src = Logs.Src.create "ox"

  module Log = (val Logs.src_log logs_src)

  (************************************************************************************************)
  (* Miscelaneous auxiliary values and functions.                                                 *)
  (************************************************************************************************)

  let all_actions = Action_set.of_list Action.all

  let num_actions = Action_set.cardinal all_actions

  (************************************************************************************************)
  (* Generic roulette-wheel selection.                                                            *)
  (************************************************************************************************)

  let select_via_roulette_wheel ~quantity ~get_weight set =
    Log.debug (fun m -> m "select_via_roulette_wheel: quantity=%d, #set=%d" quantity (Identifier_dict.length set));
    let sum =
      Identifier_dict.fold set ~init:0. ~f:(fun ~key:_ ~data:cl sum ->
        let weight = get_weight cl in
        Classifier.set_weight cl weight;
        sum +. weight
    )
    in
    let rec pick_classifiers acc_cl sum quantity =
      match sum, quantity with
      | 0., _ | _, 0 ->
          acc_cl
      | sum, quantity ->
          let target = Random.float sum in
          let seq = Identifier_dict.to_seq set in
          let rec loop_until_target acc seq =
            match seq () with
            | Seq.Nil ->
                acc_cl
            | Seq.Cons ((_, cl), seq) ->
                let weight = Classifier.(cl.weight) in
                let acc = acc +. weight in
                match acc > target with
                | true ->
                    Classifier.set_weight cl 0.;
                    pick_classifiers (cl :: acc_cl) (sum -. weight) (quantity - 1)
                | false ->
                    loop_until_target acc seq
          in
          loop_until_target 0. seq
    in
    pick_classifiers [] sum quantity

  (************************************************************************************************)
  (* Inserting/removing classifiers from population.                                              *)
  (************************************************************************************************)

  (* Routine [INSERT IN POPULATION] from page 267. *)
  let insert_into_population (Classifier.{ identifier; numerosity = n1; _ } as classifier) { set; numerosity } =
    Log.debug (fun m ->
      m "insert_into_population: classifier=%a, #population=%d, numerosity=%d"
      Identifier.pp identifier (Identifier_dict.length set) numerosity
    );
    match Identifier_dict.find_opt set identifier with
    | Some (Classifier.{ numerosity = n2; _ } as existing) ->
        Log.debug (fun m -> m "insert_into_population: Classifier already present; updating numerosity.");
        Classifier.update ~numerosity:(n1 + n2) existing;
        { set; numerosity = numerosity + n1 }
    | None ->
        Log.debug (fun m -> m "insert_into_population: Classifier not present; actually inserting.");
        Identifier_dict.add set ~key:identifier ~data:classifier;
        { set; numerosity = numerosity + n1 }

  (* Routine [DELETE FROM POPULATION] from page 268. *)
  let cull_population ~config ({ set; numerosity } as population) =
    Log.debug (fun m -> m "cull_population: #set=%d, numerosity=%d" (Identifier_dict.length set) numerosity);
    let Config.{ max_population_size; deletion_threshold; fitness_threshold; _ } = config in
    match numerosity - max_population_size with
    | excess when excess <= 0 ->
        Log.debug (fun m -> m "cull_population: Not culling because numerosity < max_population_size");
        population
    | excess ->
        Log.debug (fun m -> m "cull_population: Actually culling because excess=%d" excess);
        let avg_population_fitness =
          let sum = Identifier_dict.fold set ~init:0. ~f:(fun ~key:_ ~data:Classifier.{ fitness; _} sum -> sum +. fitness) in
          sum /. float_of_int numerosity
        in
        (* Routine [DELETION VOTE] from page 268. *)
        let culling_vote Classifier.{ fitness; experience; avg_action_set_size; numerosity; _ } =
          let numerosity = float_of_int numerosity in
          let scaled_fitness = fitness /. numerosity in
          let vote = avg_action_set_size *. numerosity in
          if experience > deletion_threshold && scaled_fitness < fitness_threshold *.avg_population_fitness
          then vote *. avg_population_fitness /. scaled_fitness
          else vote
        in
        let victims = select_via_roulette_wheel ~quantity:excess ~get_weight:culling_vote set in
        let remove_classifier { set; numerosity } = function
          | Classifier.{ identifier; numerosity = 1; _ } ->
              Log.debug (fun m -> m "cull_population: removing classifier=%a" Identifier.pp identifier);
              Identifier_dict.remove set identifier;
              { set; numerosity = numerosity - 1 }
          | Classifier.{ identifier; numerosity = n; _ } as victim ->
              Log.debug (fun m -> m "cull_population: updating classifier=%a" Identifier.pp identifier);
              Classifier.update ~numerosity:(n - 1) victim;
              { set; numerosity = numerosity - 1 }
        in
        let population = List.fold_left victims ~init:population ~f:remove_classifier in
        Log.debug (fun m ->
          let avg_population_fitness' =
            let sum = Identifier_dict.fold population.set ~init:0. ~f:(fun ~key:_ ~data:Classifier.{ fitness; _} sum -> sum +. fitness) in
            sum /. float_of_int population.numerosity
          in
          m "cull_population: avg_population_fitness: before=%7.5f, after=%7.5f (%s)"
            avg_population_fitness avg_population_fitness'
            (match Float.compare avg_population_fitness avg_population_fitness' with -1 -> "UP" | 1 -> "DOWN" | _ -> "EQUAL")
        );
        population

  (************************************************************************************************)
  (* Subsumption.                                                                                 *)
  (************************************************************************************************)

  (* Routine [COULD SUBSUME] from page 270. *)
  let could_subsume ~subsumption_threshold ~prediction_error_threshold Classifier.{ experience; prediction_error; _ } =
    Log.debug (fun m -> m "could_subsume");
    experience > subsumption_threshold && prediction_error < prediction_error_threshold

  (* Routine [DOES SUBSUME] from page 270. *)
  let does_subsume ~subsumption_threshold ~prediction_error_threshold ~subsumer ~subsumee =
    Log.debug (fun m -> m "does_subsume");
    Classifier.(subsumer.action = subsumee.action)
    && could_subsume ~subsumption_threshold ~prediction_error_threshold subsumer
    && Condition.is_more_general ~than:Classifier.(subsumee.condition) Classifier.(subsumer.condition)

  (* Routine [DO ACTION SET SUBSUMPTION] from page 269. *)
  let subsume_in_action_set ~subsumption_threshold ~prediction_error_threshold action_set population =
    Log.debug (fun m -> m "subsume_in_action_set");
    let action_set = Identifier_dict.filter action_set ~f:(fun ~key ~data:_ -> Identifier_dict.mem population.set key) in
    let best =
      Identifier_dict.fold action_set ~init:None ~f:(fun ~key:_ ~data:candidate best ->
        let Classifier.{ condition = c_condition; _ } = candidate in
        match could_subsume ~subsumption_threshold ~prediction_error_threshold candidate, best with
        | false, _ ->
            best
        | true, Some Classifier.{ condition; _ }
          when Condition.(genericity c_condition > genericity condition) ->
            Some candidate
        | true, Some Classifier.{ condition; _ }
          when Condition.(genericity c_condition = genericity condition) && Random.bool () ->
            Some candidate
        | true, Some _ ->
            best
        | true, None ->
            Some candidate
      )
    in
    match best with
    | None ->
        (action_set, population)
    | Some best ->
        Identifier_dict.filter_map_inplace action_set ~f:(fun ~key:identifier ~data:cl ->
          match Condition.is_more_general ~than:Classifier.(cl.condition) Classifier.(best.condition) with
          | true ->
              Classifier.(update ~numerosity:(best.numerosity + cl.numerosity) best);
              Identifier_dict.remove population.set identifier;
              None
          | false ->
              Some cl
        );
        (action_set, population)

  let insert_or_subsume ~subsumption_threshold ~prediction_error_threshold ~child ~parent1 ~parent2 population =
    Log.debug (fun m ->
      m "insert_or_subsume: parent1=%a, parent2=%a, child=%a"
        Identifier.pp (Classifier.identifier parent1)
        Identifier.pp (Classifier.identifier parent2)
        Identifier.pp (Classifier.identifier child)
    );
    if does_subsume ~subsumption_threshold ~prediction_error_threshold ~subsumer:parent1 ~subsumee:child
    then begin
      Log.debug (fun m -> m "insert_or_subsume: Parent1 subsumes child.");
      Classifier.(update ~numerosity:(parent1.numerosity + child.numerosity) parent1);
      { population with numerosity = population.numerosity + child.numerosity }
    end
    else if does_subsume ~subsumption_threshold ~prediction_error_threshold ~subsumer:parent2 ~subsumee:child
    then begin
      Log.debug (fun m -> m "insert_or_subsume: Parent2 subsumes child.");
      Classifier.(update ~numerosity:(parent2.numerosity + child.numerosity) parent2);
      { population with numerosity = population.numerosity + child.numerosity }
    end
    else begin
      Log.debug (fun m -> m "insert_or_subsume: Neither parent subsumes child.");
      insert_into_population child population
    end

  (************************************************************************************************)
  (* Genetic algorithm.                                                                           *)
  (************************************************************************************************)

  (* Routine [RUN GA] from page 265. *)
  let run_genetic_algorithm ~config ~current_time ~prediction action_set population environment =
    Log.debug (fun m -> m "run_genetic_algorithm: #action_set=%d" (Identifier_dict.length action_set));
    let Config.{
      prediction_error_threshold; age_threshold;
      crossover_probability; wildcard_probability; mutation_probability;
      offspring_fitness_multiplier; subsumption_threshold; do_offspring_subsumption; _
    } = config
    in
    let (sum_age, sum_numerosity) =
      let for_each_classifier ~key:_ ~data:Classifier.{ last_occurrence; numerosity; _ } (sum_age, sum_numerosity) =
        let sum_age = sum_age + numerosity * (current_time - last_occurrence) in
        let sum_numerosity = sum_numerosity + numerosity in
        (sum_age, sum_numerosity)
      in
      Identifier_dict.fold action_set ~init:(0, 0) ~f:for_each_classifier
    in
    let avg_age =
      match sum_numerosity with
      | 0 -> None
      | sum_numerosity -> Some (float_of_int sum_age /. float_of_int sum_numerosity)
    in
    match avg_age with
    | None ->
        Log.debug (fun m -> m "run_genetic_algorithm: action set is empty");
        population
    | Some avg_age when avg_age <= age_threshold ->
        Log.debug (fun m -> m "run_genetic_algorithm: avg_age=%2.1f, which is lower than age_threshold=%2.1f" avg_age age_threshold);
        population
    | Some _ ->
        Identifier_dict.iter action_set ~f:(fun ~key:_ ~data:cl -> Classifier.update ~last_occurrence:current_time cl);
        let (parent1, parent2) =
          match select_via_roulette_wheel ~quantity:2 ~get_weight:Classifier.fitness action_set with
          | [] -> (snd @@ Identifier_dict.random_exn action_set, snd @@ Identifier_dict.random_exn action_set)
          | [ parent1 ] -> (parent1, parent1)
          | [ parent1; parent2 ] -> (parent1, parent2)
          | _ -> assert false
        in
        let Classifier.{ condition = c1; action = a1; prediction_error = e1; fitness = f1; _ } = parent1 in
        let Classifier.{ condition = c2; action = a2; prediction_error = e2; fitness = f2; _ } = parent2 in
        let maybe_mutate_action action =
          if Random.float 1. < mutation_probability
          then Action_set.random_exn all_actions
          else action
        in
        let action1 = maybe_mutate_action a1 in
        let action2 = maybe_mutate_action a2 in
        let (child1, child2) =
          match (Random.float 1. < crossover_probability) && not (Classifier.equal parent1 parent2) with
          | true ->
              Log.debug (fun m ->
                m "run_genetic_algorithm: Applying crossover between %a and %a"
                  Identifier.pp (Classifier.identifier parent1)
                  Identifier.pp (Classifier.identifier parent2)
              );
              let (condition1, condition2) =
                Condition.crossover_with_mutation ~mutation_probability ~wildcard_probability ~environment c1 c2
              in
              let fitness = offspring_fitness_multiplier *. (f1 +. f2) /. 2. in
              let prediction_error = (e1 +. e2) /. 2. in
              let child1 =
                Classifier.clone
                  ~condition:condition1 ~action:action1 ~prediction ~prediction_error
                  ~fitness ~experience:0 ~numerosity:1 parent1
              in
              let child2 =
                Classifier.clone
                  ~condition:condition2 ~action:action2 ~prediction ~prediction_error
                  ~fitness ~experience:0 ~numerosity:1 parent2
              in
              (child1, child2)
          | false ->
              Log.debug (fun m ->
                m "run_genetic_algorithm: Cloning %a and %a"
                  Identifier.pp (Classifier.identifier parent1)
                  Identifier.pp (Classifier.identifier parent2)
              );
              let condition1 = Condition.clone_with_mutation ~mutation_probability ~wildcard_probability ~environment c1 in
              let condition2 = Condition.clone_with_mutation ~mutation_probability ~wildcard_probability ~environment c2 in
              let child1 =
                Classifier.clone
                  ~condition:condition1 ~action:action1 ~prediction
                  ~fitness:(offspring_fitness_multiplier *. f1) ~experience:0 ~numerosity:1 parent1
              in
              let child2 =
                Classifier.clone
                  ~condition:condition2 ~action:action2 ~prediction
                  ~fitness:(offspring_fitness_multiplier *. f2) ~experience:0 ~numerosity:1 parent2
              in
              (child1, child2)
        in
        match do_offspring_subsumption with
        | true ->
            population
            |> insert_or_subsume ~subsumption_threshold ~prediction_error_threshold ~child:child1 ~parent1 ~parent2
            |> insert_or_subsume ~subsumption_threshold ~prediction_error_threshold ~child:child2 ~parent1 ~parent2
        | false ->
            population
            |> insert_into_population child1
            |> insert_into_population child2

  (************************************************************************************************)
  (* [GENERATE MATCH SET].                                                                        *)
  (************************************************************************************************)

  (* Routine [GENERATE COVERING CLASSIFIER] from page 261. *)
  let generate_covering_classifier ~config ~current_time used_actions environment =
    Log.debug (fun m -> m "generate_covering_classifier: #used_actions=%d" (Action_set.cardinal used_actions));
    let Config.{ classifier_initialization; wildcard_probability; _ } = config in
    let Config.{ initial_prediction; initial_prediction_error; initial_fitness } = classifier_initialization in
    let unused_actions = Action_set.diff all_actions used_actions in
    let action = Action_set.random_exn unused_actions in
    let condition = Condition.make_from_environment ~wildcard_probability environment in
    Classifier.make
      ~condition
      ~action
      ~prediction:initial_prediction
      ~prediction_error:initial_prediction_error
      ~fitness:initial_fitness
      ~last_occurrence:current_time
      ~experience:0
      ~avg_action_set_size:1.
      ~numerosity:1
      ~accuracy:1. (* Dummy value, because proper accuracy will be computed in {!update_classifier_accuracy}. *)

  (* Routine [GENERATE MATCH SET] from page 260.
     Unlike in the paper, for performance reasons we don't cull the population,
     neither do we start over after generating each covering classifier. *)
  let generate_match_set ~config ~current_time population environment =
    Log.debug (fun m -> m "generate_match_set");
    let effective_min_actions = Option.value ~default:num_actions Config.(config.min_actions) in
    let match_set = Identifier_dict.create 32 in
    let used_actions =
      let process_classifier ~key ~data used_actions =
        let Classifier.{ condition; action; _ } = data in
        if Condition.matches condition environment
        then (Identifier_dict.add match_set ~key ~data; Action_set.add action used_actions)
        else used_actions
      in
      Identifier_dict.parallel_fold
        ~init:Action_set.empty
        ~neutral:Action_set.empty
        ~f:process_classifier
        ~combine:Action_set.union
        population.set
    in
    let rec loop_until_enough_actions population used_actions =
      match Action_set.cardinal used_actions >= effective_min_actions with
      | true ->
          (population, match_set)
      | false ->
          let Classifier.{ identifier; action; _ } as cl = generate_covering_classifier ~config ~current_time used_actions environment in
          Identifier_dict.add match_set ~key:identifier ~data:cl;
          let population = insert_into_population cl population in
          let used_actions = Action_set.add action used_actions in
          loop_until_enough_actions population used_actions
    in
    loop_until_enough_actions population used_actions

  (************************************************************************************************)
  (* [GENERATE PREDICTION ARRAY].                                                                 *)
  (************************************************************************************************)

  (* Routine [GENERATE PREDICTION ARRAY] from page 262. *)
  let generate_predictions match_set =
    Log.debug (fun m -> m "generate_predictions: #match_set=%d" (Identifier_dict.length match_set));
    let raw_predictions =
      let process_classifier ~key:_ ~data:Classifier.{ action; prediction; fitness; _ } map =
        Action_map.update map ~key:action ~f:(function
          | None -> Some (prediction *. fitness, fitness)
          | Some (acc_prediction, acc_fitness) -> Some (acc_prediction +. prediction *. fitness, acc_fitness +. fitness)
        )
      in
      Identifier_dict.fold match_set ~init:Action_map.empty ~f:process_classifier
    in
    let normalise_raw_prediction ~key:action ~data:(acc_prediction, acc_fitness) (num_predictions, predictions) =
      (* We don't want to divide by zero. Besides, when the accumulated
         fitness is zero the accumulated prediction is also zero. *)
      match acc_fitness with
      | 0. ->
          (num_predictions + 1, (action, 0.) :: predictions)
      | acc_fitness ->
          let normalised_prediction = acc_prediction /. acc_fitness in
          (num_predictions + 1, (action, normalised_prediction) :: predictions)
    in
    Action_map.fold raw_predictions ~init:(0, []) ~f:normalise_raw_prediction

  (************************************************************************************************)
  (* [SELECT ACTION].                                                                             *)
  (************************************************************************************************)

  let select_random_action ~num_predictions predictions =
    Log.debug (fun m -> m "select_random_action: #predictions=%d" num_predictions);
    assert (num_predictions = List.length predictions);
    List.nth predictions (Random.int num_predictions)

  let select_best_action predictions =
    Log.debug (fun m -> m "select_best_action: #predictions=%d" (List.length predictions));
    let find_best best (action, prediction) =
      match best with
      | None -> Some (action, prediction)
      | Some (_, prediction') when prediction' < prediction -> Some (action, prediction)
      | Some _ -> best
    in
    Option.get @@ List.fold_left ~f:find_best ~init:None predictions

  (************************************************************************************************)
  (* [GENERATE ACTION SET].                                                                       *)
  (************************************************************************************************)

  (* Routine [GENERATE ACTION SET] from page 263. *)
  let generate_action_set action match_set =
    Log.debug (fun m -> m "generate_action_set: action=%s, #match_set=%d" (Action.to_string action) (Identifier_dict.length match_set));
    Identifier_dict.filter match_set ~f:(fun ~key:_ ~data:cl -> Action.equal action Classifier.(cl.action))

  (************************************************************************************************)
  (* [UPDATE SET].                                                                                *)
  (************************************************************************************************)

  let update_classifier_accuracy ~config ~payoff ~action_set_numerosity classifier =
    Log.debug (fun m -> m "update_classifier_accuracy: classifier=%a" Identifier.pp (Classifier.identifier classifier));
    let Config.{ learning_rate; accuracy_coefficient; accuracy_power; prediction_error_threshold; _ } = config in
    let Classifier.{ experience; prediction; prediction_error; avg_action_set_size; _ } = classifier in
    let experience = experience + 1 in
    let experience' = float_of_int experience in
    let payoff_diff = payoff -. prediction in
    let error_diff = Float.abs payoff_diff -. prediction_error in
    let numerosity_diff = action_set_numerosity -. avg_action_set_size in
    let (prediction, prediction_error, avg_action_set_size) =
      match experience' < 1. /. learning_rate with
      | true ->
          let prediction_error = prediction_error +. error_diff /. experience' in
          let prediction = prediction +. payoff_diff /. experience' in
          let avg_action_set_size = avg_action_set_size +. numerosity_diff /. experience' in
          (prediction, prediction_error, avg_action_set_size)
      | false ->
          let prediction_error = prediction_error +. learning_rate *. error_diff in
          let prediction = prediction +. learning_rate *. payoff_diff in
          let avg_action_set_size = avg_action_set_size +. learning_rate *. numerosity_diff in
          (prediction, prediction_error, avg_action_set_size)
    in
    let accuracy =
      match prediction_error <= prediction_error_threshold with
      | true -> 1.
      | false -> accuracy_coefficient *. (( prediction_error /. prediction_error_threshold) ** ( -. accuracy_power))
    in
    Classifier.update ~experience ~prediction ~prediction_error ~avg_action_set_size ~accuracy classifier

  let update_classifier_fitness ~config ~total_accuracy (Classifier.{ fitness; numerosity; accuracy; _ } as classifier) =
    Log.debug (fun m -> m "update_classifier_fitness: classifier=%a" Identifier.pp (Classifier.identifier classifier));
    let Config.{ learning_rate; _ } = config in
    let relative_accuracy = accuracy *. float_of_int numerosity /. total_accuracy in
    let fitness = fitness +. learning_rate *. (relative_accuracy -. fitness) in
    Classifier.update ~fitness classifier

  let update_action_set ~config ~payoff action_set population =
    Log.debug (fun m -> m "update_action_set: payoff=%6.1f, #action_set=%d" payoff (Identifier_dict.length action_set));
    let Config.{ subsumption_threshold; prediction_error_threshold; do_action_set_subsumption; _ } = config in
    let action_set_numerosity =
      Identifier_dict.fold action_set ~init:0 ~f:(fun ~key:_ ~data:Classifier.{ numerosity; _} sum -> sum + numerosity)
      |> float_of_int
    in
    Identifier_dict.iter action_set ~f:(fun ~key:_ ~data:cl -> update_classifier_accuracy ~config ~payoff ~action_set_numerosity cl);
    let total_accuracy =
      Identifier_dict.fold action_set ~init:0. ~f:(fun ~key:_ ~data:Classifier.{ accuracy; numerosity; _} sum -> sum +. accuracy *. float_of_int numerosity)
    in
    Identifier_dict.iter action_set ~f:(fun ~key:_ ~data:cl -> update_classifier_fitness ~config ~total_accuracy cl);
    match do_action_set_subsumption with
    | true -> subsume_in_action_set ~subsumption_threshold ~prediction_error_threshold action_set population
    | false -> (action_set, population)

  (************************************************************************************************)
  (* [RUN EXPERIMENT].                                                                            *)
  (************************************************************************************************)

  let create ~config =
    Log.debug (fun m -> m "create");
    {
      config;
      current_time = 0;
      population = { set = Identifier_dict.create Config.(config.max_population_size); numerosity = 0 };
      previous = None;
      state = Awaiting_environment;
    }

  let get_stats { population = { set; numerosity }; _ } =
    {
      population_size = Identifier_dict.length set;
      population_numerosity = numerosity;
    }

  let get_config { config; _ } =
    config

  let update_config ~config learner =
    Log.debug (fun m -> m "update_config");
    learner.config <- config

  (* First part (lines 3-8) of routine [RUN EXPERIMENT] from page 260. *)
  let provide_environment ({ config; current_time; population; state; _ } as learner) environment =
    Log.debug (fun m -> m "provide_environment");
    match state with
    | After_environment _ ->
        raise Wrong_state
    | Awaiting_environment | After_intermediate_feedback _ ->
        let current_time = current_time + 1 in
        Log.debug (fun m ->
          m "provide_environment: current_time=%d, #population=%d, numerosity=%d"
            current_time (Identifier_dict.length population.set) population.numerosity
        );
        let (population, match_set) = generate_match_set ~config ~current_time population environment in
        let (num_predictions, predictions) = generate_predictions match_set in
        let best_action_with_prediction = lazy (select_best_action predictions) in
        let do_exploration = Random.float 1. < Config.(config.exploration_probability) in
        let (action, _prediction) =
          match do_exploration with
          | true -> select_random_action ~num_predictions predictions
          | false -> Lazy.force best_action_with_prediction
        in
        let action_set = generate_action_set action match_set in
        let state = After_environment { environment; action_set; do_exploration; best_action_with_prediction } in
        learner.current_time <- current_time;
        learner.population <- population;
        learner.state <- state;
        action

  let apply_previous_step ~config current_time best_action_with_prediction population previous =
    let Config.{ discount_factor; do_ga_only_when_exploring; _ } = config in
    let { previous_environment; previous_action_set; previous_reward; previous_do_exploration } = previous in
    match previous_do_exploration || not do_ga_only_when_exploring with
    | true ->
        let max_prediction = snd (Lazy.force best_action_with_prediction) in
        let payoff = previous_reward +. discount_factor *. max_prediction in
        let (previous_action_set', population) = update_action_set ~config ~payoff previous_action_set population in
        run_genetic_algorithm ~config ~current_time ~prediction:payoff previous_action_set' population previous_environment
    | false ->
        population

  (* Second part (lines 9-22) of routine [RUN EXPERIMENT] from page 260.
     Note that we split into two separate functions the code for intermediate and final feedback.
     This function pertains only to the former. *)
  let provide_intermediate_feedback ~reward ({ config; current_time; population; previous; state } as learner) =
    match state with
    | Awaiting_environment | After_intermediate_feedback _ ->
        raise Wrong_state
    | After_environment ({ environment; action_set; do_exploration; best_action_with_prediction } as environment_payload) ->
        Log.debug (fun m ->
          m "provide_intermediate_feedback: current_time=%d, reward=%2.1f, #population=%d, numerosity=%d"
            current_time reward (Identifier_dict.length population.set) population.numerosity
        );
        let population = Option.value_map ~default:population ~f:(apply_previous_step ~config current_time best_action_with_prediction population) previous in
        let previous = {
          previous_environment = environment;
          previous_action_set = action_set;
          previous_reward = reward;
          previous_do_exploration = do_exploration;
        }
        in
        learner.population <- cull_population ~config population;
        learner.previous <- Some previous;
        learner.state <- After_intermediate_feedback environment_payload

  (* Second part (lines 9-22) of routine [RUN EXPERIMENT] from page 260.
     Note that we split into two separate functions the code for intermediate and final feedback.
     This function pertains only to the latter. *)
  let provide_final_feedback ~reward ({ config; current_time; population; previous; state } as learner) =
    let actually_provide_final_feedback { environment; action_set; do_exploration; best_action_with_prediction } =
      Log.debug (fun m ->
        m "provide_final_feedback: current_time=%d, reward=%2.1f, #population=%d, numerosity=%d"
          current_time reward (Identifier_dict.length population.set) population.numerosity
      );
      let population = Option.value_map ~default:population ~f:(apply_previous_step ~config current_time best_action_with_prediction population) previous in
      let (action_set', population) = update_action_set ~config ~payoff:reward action_set population in
      let population =
        if do_exploration || not Config.(config.do_ga_only_when_exploring)
        then run_genetic_algorithm ~config ~current_time ~prediction:reward action_set' population environment
        else population
      in
      learner.population <- cull_population ~config population;
      learner.previous <- None;
      learner.state <- Awaiting_environment
    in
    match state with
    | Awaiting_environment ->
        raise Wrong_state
    | After_environment environment_payload ->
        actually_provide_final_feedback environment_payload
    | After_intermediate_feedback environment_payload ->
        actually_provide_final_feedback environment_payload
end
