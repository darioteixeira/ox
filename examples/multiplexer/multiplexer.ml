module Array = ArrayLabels
module Config = Ox.Config
module Environment = Ox.Environment
module Sensors = Ox.Sensors

(* This module is going to be used as an argument to the functor
   that creates the Ox learner. We use it to define the sensors,
   i.e., the representation of the state of the environment.
   In the case of the multiplexer, all inputs are booleans,
   which is why we are declaring the use of [Sensors.binary].
*)
module Sensors_def = struct
  type sensors = bool array -> unit

  let sensors = Sensors.[ binary ]
end

(* This module is also going to be used as an argument to the functor
   that creates the Ox learner. We use it to define the set of possible
   actions that the learner may take at each step.
   In the case of the multiplexer, there are only two possible actions.
*)
module Action = struct
  include Bool

  let to_string = function
    | true -> "1"
    | false -> "0"

  let of_string = function
    | "1" -> true
    | "0" -> false
    | str -> invalid_arg ("Action.of_string: " ^ str)

  let to_yojson v =
    `Bool v

  let of_yojson = function
    | `Bool b -> Ok b
    | _ -> Error "Action.t"

  let all = [ false; true ]
end

(* This module is the third and final argument to the functor that creates
   the Ox learner. It defines the data structure used to hold the population
   of classifiers. In this case we want to parallelise the work across four
   different CPU cores using OCaml 5 and Domainslib.
*)
(*
module Dict = Ox.Multicore_dict.Make (struct
  let num_domains = 4
  let task_pool = Domainslib.Task.setup_pool ~num_domains:(num_domains - 1) ()
end)
*)
(* Uncomment the line below (and comment out the module definition immediately above)
   to use the single core version of the Ox learner.
*)
module Dict = Ox.Singlecore_dict.Make

(* Create the learner module by invoking the [Ox.Learner.Make] functor using
   the modules defined above as parameters.
*)
module Learner = Ox.Learner.Make (Sensors_def) (Action) (Dict)

let bool_array_of_int ~length n =
  Array.init length ~f:(fun idx -> (1 lsl idx) land n <> 0)

let random_bool_array ~length =
  Array.init length ~f:(fun _idx -> Random.bool ())

let generate_random_environment ~selector_width =
  assert (selector_width <= 61); (* Limit due to the logical shift left in the line below. *)
  let num_inputs = 1 lsl selector_width in
  let selector = Random.int num_inputs in
  let selector_array = bool_array_of_int ~length:selector_width selector in
  let input_array = random_bool_array ~length:num_inputs in
  let correct_output = input_array.( selector ) in
  let environment = Array.concat [ input_array; selector_array ] in
  (environment, correct_output)

let run_batch ~selector_width ~batch_num ~batch_size ~switchover_iteration config learner =
  let rec loop local_iteration sum_reward =
    match local_iteration < batch_size with
    | false ->
        sum_reward
    | true ->
        let iteration = batch_num * batch_size + local_iteration in
        let (environment, correct_action) = generate_random_environment ~selector_width in
        let exploration_probability =
          let mult = Float.min 1. (float_of_int iteration /. float_of_int switchover_iteration) in
          0.7 -. mult *. 0.7
        in
        let () = Learner.update_config ~config:{ config with exploration_probability } learner in
        let action = Learner.provide_environment learner Environment.[ environment ] in
        let reward =
          match Bool.equal action correct_action with
          | true -> 1000.
          | false -> -1000.
        in
        Learner.provide_final_feedback ~reward learner;
        loop (local_iteration + 1) (sum_reward +. reward)
  in
  let sum_reward = loop 0 0. in
  let avg_reward = sum_reward /. float_of_int batch_size in
  let Learner.{ population_size; population_numerosity } = Learner.get_stats learner in
  Format.printf "batch_num=%4d, avg_reward=%6.1f, population=(%d/%d)\n%!" batch_num avg_reward population_size population_numerosity

let main ~selector_width ~num_batches ~batch_size ~switchover_iteration =
  let config = Config.{ default with max_population_size = 10_000 } in
  let learner = Learner.create ~config in
  for batch_num = 0 to num_batches - 1 do
    run_batch ~selector_width ~batch_num ~batch_size ~switchover_iteration config learner
  done

let () =
  Random.self_init ();
  Logs.set_reporter (Logs.format_reporter ());
  Logs.Src.set_level Learner.logs_src (Some Info);
  main ~selector_width:4 ~num_batches:50 ~batch_size:10_000 ~switchover_iteration:300_000
