open Prelude

module Array = ArrayLabels

include Condition_intf

module Elements = struct
  type 'a specificity =
    | Specific of 'a
    | Wildcard

  type _ t =
    | [] : unit t
    | ( :: ) : 'a specificity array * 'b t -> ('a array * 'b) t
end

module Make (Sensors_def : Sensors.DEF) : S with module Sensors_def = Sensors_def = struct

  module Sensors_def = Sensors_def

  type 'a condition = {
    elements : 'a Elements.t;
    genericity : int; (* Cached computed value: number of wildcards in the elements. *)
  }

  type t = Sensors_def.sensors condition

  let default_inter_group_separator = ';'

  let wildcard = "#"

  let to_string ?intra_group_separator ?(inter_group_separator = default_inter_group_separator) { elements; _ } =
    let buf = Buffer.create 128 in
    let process_hd : type a. a Sensors.sensor -> a Elements.specificity array -> unit =
      fun hd_sensors hd_elements ->
        let (module Sensor) = hd_sensors in
        let to_string = function
          | Elements.Specific v -> Sensor.to_string v
          | Elements.Wildcard -> wildcard
        in
        let penultimate_idx = Array.length hd_elements - 2 in
        Array.iteri hd_elements ~f:(fun idx element ->
          Buffer.add_string buf (to_string element);
          match intra_group_separator with
          | Some sep when idx <= penultimate_idx -> Buffer.add_char buf sep
          | _ -> ()
        )
    in
    let rec iter : type a. a Sensors.t -> a Elements.t -> unit =
      fun sensors elements ->
        match sensors, elements with
        | Sensors.[], Elements.[] ->
            ()
        | Sensors.[ hd_sensors ], Elements.[ hd_elements ] ->
            process_hd hd_sensors hd_elements
        | Sensors.(hd_sensors :: tl_sensors), Elements.(hd_elements :: tl_elements) ->
            process_hd hd_sensors hd_elements;
            Buffer.add_char buf inter_group_separator;
            iter tl_sensors tl_elements
    in
    iter Sensors_def.sensors elements;
    Buffer.contents buf

  let of_string ?intra_group_separator ?(inter_group_separator = default_inter_group_separator) str =
    let rec loop : type a. a Sensors.t -> string list -> a condition =
      fun sensors groups ->
        match sensors, groups with
        | Sensors.[], [] ->
            { genericity = 0; elements = Elements.[] }
        | Sensors.[], (_ :: _) ->
            invalid_arg "Condition.of_string: String contains too many groups"
        | Sensors.(hd_sensors :: tl_sensors), (hd_groups :: tl_groups) ->
            let (module Sensor) = hd_sensors in
            let str_elements =
              match intra_group_separator with
              | Some sep -> String.split_on_char sep hd_groups
              | None -> hd_groups |> String.to_seq |> Seq.map (String.make 1) |> List.of_seq
            in
            let process_element acc = function
              | str when String.equal str wildcard -> (acc + 1, Elements.Wildcard)
              | str -> (acc, Elements.Specific (Sensor.of_string str))
            in
            let (hd_genericity, hd_elements) =
              str_elements
              |> Array.of_list
              |> Array.fold_left_map ~init:0 ~f:process_element
            in
            let tl = loop tl_sensors tl_groups in
            { genericity = hd_genericity + tl.genericity; elements = Elements.(hd_elements :: tl.elements) }
        | Sensors.(_ :: _), [] ->
            invalid_arg "Condition.of_string: String contains too few groups"
    in
    let groups = String.split_on_char inter_group_separator str in
    loop Sensors_def.sensors groups

  let genericity { genericity; _ } =
    genericity

  let matches { elements; _ } environment =
    let rec loop : type a. a Sensors.t -> a Elements.t -> a Environment.t -> bool =
      fun sensors elements environment ->
        match sensors, elements, environment with
        | Sensors.[],
          Elements.[],
          Environment.[] ->
            true
        | Sensors.(hd_sensors :: tl_sensors),
          Elements.(hd_elements :: tl_elements),
          Environment.(hd_environment :: tl_environment) ->
            let (module Sensor) = hd_sensors in
            let does_match env = function
              | Elements.Wildcard -> true
              | Elements.Specific v when Sensor.equal v env -> true
              | Elements.Specific _ -> false
            in
            match Array.for_all2 ~f:does_match hd_environment hd_elements with
            | true -> loop tl_sensors tl_elements tl_environment
            | false -> false
    in
    loop Sensors_def.sensors elements environment

  let is_more_general ~than subject =
    let rec loop : type a. a Sensors.t -> a Elements.t -> a Elements.t -> bool =
      fun sensors subject_elements than_elements ->
        match sensors, subject_elements, than_elements with
        | Sensors.[],
          Elements.[],
          Elements.[] ->
            true
        | Sensors.(hd_sensors :: tl_sensors),
          Elements.(hd_subject_elements :: tl_subject_elements),
          Elements.(hd_than_elements :: tl_than_elements) ->
            let (module Sensor) = hd_sensors in
            let is_element_more_general subject_elem than_elem =
              match subject_elem, than_elem with
              | Elements.(Wildcard, _) -> true
              | Elements.(_, Wildcard) -> true
              | Elements.(Specific v1, Specific v2) -> Sensor.equal v1 v2
            in
            match Array.for_all2 ~f:is_element_more_general hd_subject_elements hd_than_elements with
            | true -> loop tl_sensors tl_subject_elements tl_than_elements
            | false -> false
    in
    subject.genericity > than.genericity
    && loop Sensors_def.sensors subject.elements than.elements

  let make_from_environment ~wildcard_probability environment =
    let rec loop : type a. a Sensors.t -> a Environment.t -> a condition =
      fun sensors environment ->
        match sensors, environment with
        | Sensors.[], Environment.[] ->
            { genericity = 0; elements = Elements.[] }
        | Sensors.(hd_sensors :: tl_sensors), Environment.(hd_environment :: tl_environment) ->
            let (module Sensor) = hd_sensors in
            let (hd_genericity, hd_elements) =
              Array.fold_left_map hd_environment ~init:0 ~f:(fun acc env ->
                if Random.float 1.0 < wildcard_probability
                then (acc + 1, Elements.Wildcard)
                else (acc, Elements.Specific env)
              )
            in
            let tl = loop tl_sensors tl_environment in
            { genericity = hd_genericity + tl.genericity; elements = Elements.(hd_elements :: tl.elements) }
    in
    loop Sensors_def.sensors environment

  let mutate (type sensors) ~mutation_probability ~wildcard_probability (module Sensor : Sensors.SENSOR with type t = sensors) acc elem maybe_env =
    let elem' =
      match Random.float 1.0 < mutation_probability with
      | false ->
          elem
      | true ->
          match elem, maybe_env with
          | Elements.Wildcard, None -> Elements.Specific (Sensor.random ())
          | Elements.Wildcard, Some env -> Elements.Specific env
          | Elements.Specific _, _ when Random.float 1.0 < wildcard_probability -> Elements.Wildcard
          | Elements.Specific v, None -> Elements.Specific (Sensor.random ~exclude:v ())
          | Elements.Specific _, Some env -> Elements.Specific env
    in
    let acc' =
      match elem' with
      | Elements.Wildcard -> acc + 1
      | Elements.Specific _ -> acc
    in
    (acc', elem')

  let clone_with_mutation ~mutation_probability ~wildcard_probability ?environment { elements; _ } =
    let rec loop : type a. a Sensors.t -> a Elements.t -> a Environment.t option -> a condition =
      fun sensors elements environment ->
        match sensors, elements, environment with
        | Sensors.[], Elements.[], _ ->
            { genericity = 0; elements = Elements.[] }
        | Sensors.(hd_sensors :: tl_sensors),
          Elements.(hd_elements :: tl_elements),
          None ->
            let (hd_genericity, hd_elements) =
              Array.fold_left_map hd_elements ~init:0 ~f:(fun acc elem ->
                mutate ~mutation_probability ~wildcard_probability hd_sensors acc elem None
              )
            in
            let tl = loop tl_sensors tl_elements None in
            {
              genericity = hd_genericity + tl.genericity;
              elements = Elements.(hd_elements :: tl.elements);
            }
        | Sensors.(hd_sensors :: tl_sensors),
          Elements.(hd_elements :: tl_elements),
          Some Environment.(hd_environment :: tl_environment) ->
            let (hd_genericity, hd_elements) =
              Array.fold_left_map2 hd_elements hd_environment ~init:0 ~f:(fun acc elem env ->
                mutate ~mutation_probability ~wildcard_probability hd_sensors acc elem (Some env)
              )
            in
            let tl = loop tl_sensors tl_elements (Some tl_environment) in
            {
              genericity = hd_genericity + tl.genericity;
              elements = Elements.(hd_elements :: tl.elements);
            }
    in
    loop Sensors_def.sensors elements environment

  let crossover { elements = elements1; _ } { elements = elements2; _ } =
    let apply_mask mask elements_true elements_false =
      Array.fold_left_map3 mask elements_true elements_false ~init:0 ~f:(fun acc mask elem1 elem2 ->
        let elem = if mask then elem1 else elem2 in
        let acc =
          match elem with
          | Elements.Wildcard -> acc + 1
          | Elements.Specific _ -> acc
        in
        (acc, elem)
      )
    in
    let rec loop : type a. a Elements.t -> a Elements.t -> a condition * a condition =
      fun elements1 elements2 ->
        match elements1, elements2 with
        | Elements.[], Elements.[] ->
            let empty = { elements = Elements.[]; genericity = 0 } in
            (empty, empty)
        | Elements.(hd_elements1 :: tl_elements1), Elements.(hd_elements2 :: tl_elements2) ->
            let crossover_mask = Array.(init (length hd_elements1) ~f:(fun _ -> Random.bool ())) in
            let (hd1_genericity, hd1_elements) = apply_mask crossover_mask hd_elements1 hd_elements2 in
            let (hd2_genericity, hd2_elements) = apply_mask crossover_mask hd_elements2 hd_elements1 in
            let (tl1, tl2) = loop tl_elements1 tl_elements2 in
            let v1 = {
              genericity = hd1_genericity + tl1.genericity;
              elements = Elements.(hd1_elements :: tl1.elements);
            }
            in
            let v2 = {
              genericity = hd2_genericity + tl2.genericity;
              elements = Elements.(hd2_elements :: tl2.elements);
            }
            in
            (v1, v2)
    in
    loop elements1 elements2

  let crossover_with_mutation ~mutation_probability ~wildcard_probability ?environment condition1 condition2 =
    let (condition3, condition4) = crossover condition1 condition2 in
    let condition3 = clone_with_mutation ~mutation_probability ~wildcard_probability ?environment condition3 in
    let condition4 = clone_with_mutation ~mutation_probability ~wildcard_probability ?environment condition4 in
    (condition3, condition4)
end
