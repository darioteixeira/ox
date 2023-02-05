module Array = ArrayLabels
module List = ListLabels
module Config = Ox.Config
module Environment = Ox.Environment
module Sensors = Ox.Sensors
module Ternary = Sensors.Ternary

(* This module is going to be used as an argument to the functor
   that creates the Ox learner. We use it to define the sensors,
   i.e., the representation of the state of the environment.
   In the case of a Tic-tac-toe board, all inputs are ternary,
   since a position may either be a) empty, b) ours, c) theirs.
*)

module Sensors_def = struct
  type sensors = Ternary.t array -> unit

  let sensors = Sensors.[ ternary ]
end

(* This module is also going to be used as an argument to the functor
   that creates the Ox learner. We use it to define the set of possible
   actions that the learner may take at each step.
   In the case of Tic-tac-toe, there are nine possible outputs, which
   we are representing by the integers 0..8.
*)
module Action = struct
  include Int

  let of_string = int_of_string

  let to_yojson v =
    `Int v

  let of_yojson = function
    | `Int i -> Ok i
    | _ -> Error "Action.t"

  let all = List.init ~len:9 ~f:Fun.id
end

(* This module is the third and final argument to the functor that creates
   the Ox learner. It defines the data structure used to hold the population
   of classifiers. In this case we want to parallelise the work across four
   different CPU cores using OCaml 5 and Domainslib.
*)
module Hashtbl = Ox.Multicore_dict.Make (struct
  let num_domains = 4
  let task_pool = Domainslib.Task.setup_pool ~num_domains:(num_domains - 1) ()
end)

(* Uncomment the line below (and comment out the module definition immediately above)
   to use the single core version of the Ox learner.
*)
(* module Dict = Ox.Singlecore_dict.Make *)

(* Create the learner module by invoking the [Ox.Learner.Make] functor using
   the modules defined above as parameters.
*)
module Learner = Ox.Learner.Make (Sensors_def) (Action) (Hashtbl)

type mark = [ `Cross | `Nought ]

type square = [ mark | `Empty ]

let equal_square v1 v2 =
  match v1, v2 with
  | `Cross, `Cross
  | `Nought, `Nought
  | `Empty, `Empty -> true
  | _ -> false

type id = Alpha | Beta

type player = {
  id : id;
  learner : Learner.t;
  mark : mark;
}

type board = {
  num_occupied : int;
  occupation : square array;
}

type outcome =
  | Tie
  | Victory of id
  | Disqualification of id

type tally = {
  num_ties : int;
  num_alpha_victories : int;
  num_beta_victories : int;
  num_alpha_disqualifications : int;
  num_beta_disqualifications : int;
  sum_num_occupied : int;
}

let is_there_winning_line mark occupation =
  let three_in_line line = List.for_all ~f:(fun pos -> equal_square occupation.( pos ) mark) line in
  let check_rows () = List.exists ~f:three_in_line [ [0; 1; 2]; [3; 4; 5]; [6; 7; 8] ] in
  let check_columns () = List.exists ~f:three_in_line [ [0; 3; 6]; [1; 4; 7]; [2; 5; 8] ] in
  let check_diagonals () = List.exists ~f:three_in_line [ [0; 4; 8]; [2; 4; 6] ] in
  check_rows () || check_columns () || check_diagonals ()

let environment_of_occupation ~mark occupation =
  let squares = Array.map occupation ~f:(function
    | `Empty -> Ternary.Unknown
    | (`Cross | `Nought) as mark' when equal_square mark mark' -> Ternary.True
    | `Cross | `Nought -> Ternary.False
  )
  in
  Environment.[ squares ]

let play_game ~cross:(id_cross, learner_cross) ~nought:(id_nought, learner_nought) =
  let rec loop player adversary board =
    match board with
    | { num_occupied; _ } when num_occupied = 9 ->
        Learner.provide_final_feedback ~reward:500. player.learner;
        Learner.provide_final_feedback ~reward:500. adversary.learner;
        (Tie, num_occupied)
    | { num_occupied; occupation } ->
        let mark = (player.mark :> square) in
        let environment = environment_of_occupation ~mark occupation in
        let action = Learner.provide_environment player.learner environment in
        match occupation.( action ) with
        | `Cross | `Nought ->
            (* Invalid move: position already occupied. *)
            Learner.provide_final_feedback ~reward:(-1000.) player.learner;
            Learner.provide_final_feedback ~reward:0. adversary.learner;
            (Disqualification player.id, num_occupied)
        | `Empty ->
            (* Valid move: let's check if it's also a winning move. *)
            occupation.( action) <- mark;
            match is_there_winning_line mark occupation with
            | true ->
                Learner.provide_final_feedback ~reward:1000. player.learner;
                Learner.provide_final_feedback ~reward:(-1000.) adversary.learner;
                (Victory player.id, num_occupied + 1)
            | false ->
                Learner.provide_intermediate_feedback ~reward:0. player.learner;
                let board = { num_occupied = num_occupied + 1; occupation } in
                loop adversary player board
  in
  let player_cross = { id = id_cross; learner = learner_cross; mark = `Cross } in
  let player_nought = { id = id_nought; learner = learner_nought; mark = `Nought } in
  let board = { num_occupied = 0; occupation = Array.make 9 `Empty } in
  loop player_cross player_nought board

let print_tally ~batch_num ~batch_size learner_alpha learner_beta tally =
  let perc x =
    100. *. float_of_int x /. float_of_int batch_size
  in
  let sprint_stats learner =
    let Learner.{ population_size; population_numerosity } = Learner.get_stats learner in
    Printf.sprintf "(%d/%d)" population_size population_numerosity
  in
  Format.printf
    "Batch %4d: μN=%6.2f, %%T=%6.2f, %%αV=%6.2f, %%βV=%6.2f, %%αD=%6.2f, %%βD=%6.2f, α#=%s, β#=%s\n%!"
    batch_num
    (float_of_int tally.sum_num_occupied /. float_of_int batch_size)
    (perc tally.num_ties)
    (perc tally.num_alpha_victories)
    (perc tally.num_beta_victories)
    (perc tally.num_alpha_disqualifications)
    (perc tally.num_beta_disqualifications)
    (sprint_stats learner_alpha)
    (sprint_stats learner_beta)

let run_batch ~batch_num ~batch_size (config_alpha, learner_alpha) (config_beta, learner_beta) =
  let player_alpha = (Alpha, learner_alpha) in
  let player_beta = (Beta, learner_beta) in
  let rec loop ~local_iteration ~tally =
    match local_iteration < batch_size with
    | false ->
        tally
    | true ->
        let iteration = batch_num * batch_size + local_iteration in
        let exploration_probability = 0.71 -. 0.7 *. float_of_int (Int.min iteration 100_000) /. 100_000. in
        Learner.update_config ~config:{ config_alpha with exploration_probability } learner_alpha;
        Learner.update_config ~config:{ config_beta with exploration_probability } learner_beta;
        let (cross, nought) =
          match iteration mod 2 = 0 with
          | false -> (player_alpha, player_beta)
          | true -> (player_beta, player_alpha)
        in
        let (outcome, num_occupied) = play_game ~cross ~nought in
        let tally = { tally with sum_num_occupied = tally.sum_num_occupied + num_occupied } in
        let tally =
          match outcome with
          | Tie -> { tally with num_ties = tally.num_ties + 1 }
          | Victory Alpha -> { tally with num_alpha_victories = tally.num_alpha_victories + 1 }
          | Victory Beta -> { tally with num_beta_victories = tally.num_beta_victories + 1 }
          | Disqualification Alpha -> { tally with num_alpha_disqualifications = tally.num_alpha_disqualifications + 1 }
          | Disqualification Beta -> { tally with num_beta_disqualifications = tally.num_beta_disqualifications + 1 }
        in
        loop ~local_iteration:(local_iteration + 1) ~tally
  in
  let tally = {
    num_ties = 0;
    num_alpha_victories = 0;
    num_beta_victories = 0;
    num_alpha_disqualifications = 0;
    num_beta_disqualifications = 0;
    sum_num_occupied = 0;
  }
  in
  let tally = loop ~local_iteration:0 ~tally in
  print_tally ~batch_num ~batch_size learner_alpha learner_beta tally

let main ~num_batches ~batch_size =
  let config = Ox.Config.{ default with learning_rate = 0.3 } in
  let config_alpha = Ox.Config.{ config with max_population_size = 50_000 } in
  let config_beta = Ox.Config.{ config with max_population_size = 50_000 } in
  let learner_alpha = Learner.create ~config:config_alpha in
  let learner_beta = Learner.create ~config:config_beta in
  for batch_num = 0 to num_batches - 1 do
    run_batch ~batch_num ~batch_size (config_alpha, learner_alpha) (config_beta, learner_beta)
  done

let () =
  Random.self_init ();
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Info);
  main ~num_batches:200 ~batch_size:1_000
