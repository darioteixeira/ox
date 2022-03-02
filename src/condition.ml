open Prelude

module Array = ArrayLabels

include Condition_intf

module Locus = struct
  type 'a t =
    | Specific of 'a
    | Wildcard
    [@@deriving eq]
end

module Groups = struct
  type _ t =
    | [] : unit t
    | ( :: ) : 'a Locus.t array * 'b t -> ('a array * 'b) t
end

module Make (Sensors_def : Sensors.DEF) : S with type sensors = Sensors_def.sensors = struct

  type sensors = Sensors_def.sensors

  type 'a condition = {
    groups : 'a Groups.t;
    genericity : int; (* Cached computed value: number of wildcards in the groups. *)
  }

  type t = sensors condition

  let make_from_environment ~wildcard_probability environment =
    let rec loop : type a. a Sensors.t -> a Environment.t -> a condition =
      fun sensors environment ->
        match sensors, environment with
        | Sensors.[], Environment.[] ->
            { genericity = 0; groups = Groups.[] }
        | Sensors.(hd_sensor :: tl_sensors), Environment.(hd_environment :: tl_environment) ->
            let (module Sensor) = hd_sensor in
            let (hd_genericity, hd_group) =
              Array.fold_left_map hd_environment ~init:0 ~f:(fun acc env ->
                if Random.float 1.0 < wildcard_probability
                then (acc + 1, Locus.Wildcard)
                else (acc, Locus.Specific env)
              )
            in
            let tl = loop tl_sensors tl_environment in
            { genericity = hd_genericity + tl.genericity; groups = Groups.(hd_group :: tl.groups) }
    in
    loop Sensors_def.sensors environment

  let genericity { genericity; _ } =
    genericity

  let matches { groups; _ } environment =
    let rec loop : type a. a Sensors.t -> a Groups.t -> a Environment.t -> bool =
      fun sensors groups environment ->
        match sensors, groups, environment with
        | Sensors.[],
          Groups.[],
          Environment.[] ->
            true
        | Sensors.(hd_sensor :: tl_sensors),
          Groups.(hd_group :: tl_groups),
          Environment.(hd_environment :: tl_environment) ->
            let (module Sensor) = hd_sensor in
            let does_match env = function
              | Locus.Wildcard -> true
              | Locus.Specific v when Sensor.equal v env -> true
              | Locus.Specific _ -> false
            in
            match Array.for_all2 ~f:does_match hd_environment hd_group with
            | true -> loop tl_sensors tl_groups tl_environment
            | false -> false
    in
    loop Sensors_def.sensors groups environment

  let is_more_general ~than subject =
    let rec loop : type a. a Sensors.t -> a Groups.t -> a Groups.t -> bool =
      fun sensors subject_groups than_groups ->
        match sensors, subject_groups, than_groups with
        | Sensors.[],
          Groups.[],
          Groups.[] ->
            true
        | Sensors.(hd_sensor :: tl_sensors),
          Groups.(hd_subject_groups :: tl_subject_groups),
          Groups.(hd_than_groups :: tl_than_groups) ->
            let (module Sensor) = hd_sensor in
            let is_locus_more_general subject_locus than_locus =
              match subject_locus, than_locus with
              | Locus.(Wildcard, _) -> true
              | Locus.(_, Wildcard) -> true
              | Locus.(Specific v1, Specific v2) -> Sensor.equal v1 v2
            in
            match Array.for_all2 ~f:is_locus_more_general hd_subject_groups hd_than_groups with
            | true -> loop tl_sensors tl_subject_groups tl_than_groups
            | false -> false
    in
    subject.genericity > than.genericity
    && loop Sensors_def.sensors subject.groups than.groups

  let mutate (type sensors) ~mutation_probability ~wildcard_probability (module Sensor : Sensors.SENSOR with type t = sensors) acc locus maybe_env =
    let locus' =
      match Random.float 1.0 < mutation_probability with
      | false ->
          locus
      | true ->
          match locus, maybe_env with
          | Locus.Wildcard, None -> Locus.Specific (Sensor.random ())
          | Locus.Wildcard, Some env -> Locus.Specific env
          | Locus.Specific _, _ when Random.float 1.0 < wildcard_probability -> Locus.Wildcard
          | Locus.Specific v, None -> Locus.Specific (Sensor.random ~exclude:v ())
          | Locus.Specific _, Some env -> Locus.Specific env
    in
    let acc' =
      match locus' with
      | Locus.Wildcard -> acc + 1
      | Locus.Specific _ -> acc
    in
    (acc', locus')

  let clone_with_mutation ~mutation_probability ~wildcard_probability ?environment { groups; _ } =
    let rec loop : type a. a Sensors.t -> a Groups.t -> a Environment.t option -> a condition =
      fun sensors groups environment ->
        match sensors, groups, environment with
        | Sensors.[], Groups.[], _ ->
            { genericity = 0; groups = Groups.[] }
        | Sensors.(hd_sensor :: tl_sensors),
          Groups.(hd_group :: tl_groups),
          None ->
            let (hd_genericity, hd_group) =
              Array.fold_left_map hd_group ~init:0 ~f:(fun acc locus ->
                mutate ~mutation_probability ~wildcard_probability hd_sensor acc locus None
              )
            in
            let tl = loop tl_sensors tl_groups None in
            {
              genericity = hd_genericity + tl.genericity;
              groups = Groups.(hd_group :: tl.groups);
            }
        | Sensors.(hd_sensor :: tl_sensors),
          Groups.(hd_group :: tl_groups),
          Some Environment.(hd_environment :: tl_environment) ->
            let (hd_genericity, hd_group) =
              Array.fold_left_map2 hd_group hd_environment ~init:0 ~f:(fun acc locus env ->
                mutate ~mutation_probability ~wildcard_probability hd_sensor acc locus (Some env)
              )
            in
            let tl = loop tl_sensors tl_groups (Some tl_environment) in
            {
              genericity = hd_genericity + tl.genericity;
              groups = Groups.(hd_group :: tl.groups);
            }
    in
    loop Sensors_def.sensors groups environment

  let crossover { groups = groups1; _ } { groups = groups2; _ } =
    let apply_mask mask groups_true groups_false =
      Array.fold_left_map3 mask groups_true groups_false ~init:0 ~f:(fun acc mask locus1 locus2 ->
        let locus = if mask then locus1 else locus2 in
        let acc =
          match locus with
          | Locus.Wildcard -> acc + 1
          | Locus.Specific _ -> acc
        in
        (acc, locus)
      )
    in
    let rec loop : type a. a Groups.t -> a Groups.t -> a condition * a condition =
      fun groups1 groups2 ->
        match groups1, groups2 with
        | Groups.[], Groups.[] ->
            let empty = { groups = Groups.[]; genericity = 0 } in
            (empty, empty)
        | Groups.(hd_group1 :: tl_groups1), Groups.(hd_group2 :: tl_groups2) ->
            let crossover_mask = Array.(init (length hd_group1) ~f:(fun _ -> Random.bool ())) in
            let (hd1_genericity, hd1_groups) = apply_mask crossover_mask hd_group1 hd_group2 in
            let (hd2_genericity, hd2_groups) = apply_mask crossover_mask hd_group2 hd_group1 in
            let (tl1, tl2) = loop tl_groups1 tl_groups2 in
            let v1 = {
              genericity = hd1_genericity + tl1.genericity;
              groups = Groups.(hd1_groups :: tl1.groups);
            }
            in
            let v2 = {
              genericity = hd2_genericity + tl2.genericity;
              groups = Groups.(hd2_groups :: tl2.groups);
            }
            in
            (v1, v2)
    in
    loop groups1 groups2

  let crossover_with_mutation ~mutation_probability ~wildcard_probability ?environment condition1 condition2 =
    let (condition3, condition4) = crossover condition1 condition2 in
    let condition3 = clone_with_mutation ~mutation_probability ~wildcard_probability ?environment condition3 in
    let condition4 = clone_with_mutation ~mutation_probability ~wildcard_probability ?environment condition4 in
    (condition3, condition4)

  let default_intra_group_separator = ','

  let default_inter_group_separator = ';'

  let wildcard = "#"

  let to_string ?intra_group_separator ?(inter_group_separator = default_inter_group_separator) { groups; _ } =
    let buf = Buffer.create 128 in
    let process_hd : type a. a Sensors.sensor -> a Locus.t array -> unit =
      fun hd_sensor hd_group ->
        let (module Sensor) = hd_sensor in
        let to_string = function
          | Locus.Specific v -> Sensor.to_string v
          | Locus.Wildcard -> wildcard
        in
        let penultimate_idx = Array.length hd_group - 2 in
        Array.iteri hd_group ~f:(fun idx locus ->
          Buffer.add_string buf (to_string locus);
          match intra_group_separator with
          | Some sep when idx <= penultimate_idx -> Buffer.add_char buf sep
          | _ -> ()
        )
    in
    let rec iter : type a. a Sensors.t -> a Groups.t -> unit =
      fun sensors groups ->
        match sensors, groups with
        | Sensors.[], Groups.[] ->
            ()
        | Sensors.[ hd_sensor ], Groups.[ hd_group ] ->
            process_hd hd_sensor hd_group
        | Sensors.(hd_sensor :: tl_sensors), Groups.(hd_group :: tl_groups) ->
            process_hd hd_sensor hd_group;
            Buffer.add_char buf inter_group_separator;
            iter tl_sensors tl_groups
    in
    iter Sensors_def.sensors groups;
    Buffer.contents buf

  let of_string ?intra_group_separator ?(inter_group_separator = default_inter_group_separator) str =
    let rec loop : type a. a Sensors.t -> string list -> a condition =
      fun sensors groups ->
        match sensors, groups with
        | Sensors.[], [] ->
            { genericity = 0; groups = Groups.[] }
        | Sensors.[], (_ :: _) ->
            invalid_arg "Condition.of_string: String contains too many groups"
        | Sensors.(hd_sensor :: tl_sensors), (hd_group :: tl_groups) ->
            let (module Sensor) = hd_sensor in
            let str_groups =
              match intra_group_separator with
              | Some sep -> String.split_on_char sep hd_group
              | None -> hd_group |> String.to_seq |> Seq.map (String.make 1) |> List.of_seq
            in
            let process_locus acc = function
              | str when String.equal str wildcard -> (acc + 1, Locus.Wildcard)
              | str -> (acc, Locus.Specific (Sensor.of_string str))
            in
            let (hd_genericity, hd_group) =
              str_groups
              |> Array.of_list
              |> Array.fold_left_map ~init:0 ~f:process_locus
            in
            let tl = loop tl_sensors tl_groups in
            { genericity = hd_genericity + tl.genericity; groups = Groups.(hd_group :: tl.groups) }
        | Sensors.(_ :: _), [] ->
            invalid_arg "Condition.of_string: String contains too few groups"
    in
    let groups = String.split_on_char inter_group_separator str in
    loop Sensors_def.sensors groups

  let to_yojson v =
    `String (to_string ~intra_group_separator:default_intra_group_separator v)

  let of_yojson = function
    | `String str -> Ok (of_string ~intra_group_separator:default_intra_group_separator str)
    | _ -> Error "Condition.t"

  let equal c1 c2 =
    let rec loop : type a. a Sensors.t -> a Groups.t -> a Groups.t -> bool =
      fun sensors groups1 groups2 ->
        match sensors, groups1, groups2 with
        | Sensors.[],
          Groups.[],
          Groups.[] ->
            true
        | Sensors.(hd_sensor :: tl_sensors),
          Groups.(hd_group1 :: tl_groups1),
          Groups.(hd_group2 :: tl_groups2) ->
            let (module Sensor) = hd_sensor in
            match Array.for_all2 ~f:(Locus.equal Sensor.equal) hd_group1 hd_group2 with
            | true -> loop tl_sensors tl_groups1 tl_groups2
            | false -> false
    in
    c1.genericity = c2.genericity
    && loop Sensors_def.sensors c1.groups c2.groups
end
