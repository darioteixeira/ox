module Array = struct
  include ArrayLabels

  let fold_left_map2 ~f ~init array1 array2 =
    match length array1, length array2 with
    | 0, 0 ->
        (init, [| |])
    | len1, len2 when len1 <> len2 ->
        invalid_arg "ArrayLabels.fold_left_map2: arrays must have the same length"
    | len, _ ->
        let (acc, elt) = f init (unsafe_get array1 0) (unsafe_get array2 0) in
        let dest = make len elt in
        let rec loop acc idx =
          match idx < len with
          | false ->
              acc
          | true ->
              let (acc, elt) = f acc (unsafe_get array1 idx) (unsafe_get array2 idx) in
              unsafe_set dest idx elt;
              loop acc (idx + 1)
        in
        (loop acc 1, dest)

  let fold_left_map3 ~f ~init array1 array2 array3 =
    match length array1, length array2, length array3 with
    | 0, 0, 0 ->
        (init, [| |])
    | len1, len2, len3 when len1 <> len2 || len1 <> len3 ->
        invalid_arg "ArrayLabels.fold_left_map3: arrays must have the same length"
    | len, _, _ ->
        let (acc, elt) = f init (unsafe_get array1 0) (unsafe_get array2 0) (unsafe_get array3 0) in
        let dest = make len elt in
        let rec loop acc idx =
          match idx < len with
          | false ->
              acc
          | true ->
              let (acc, elt) = f acc (unsafe_get array1 idx) (unsafe_get array2 idx) (unsafe_get array3 idx) in
              unsafe_set dest idx elt;
              loop acc (idx + 1)
        in
        (loop acc 1, dest)
end

module List = ListLabels

module Option = struct
  include Option

  let value_map ~default ~f = function
    | None -> default
    | Some v -> f v
end

module Result = struct
  include Result

  let map_list ~f l =
    let rec loop acc = function
      | [] ->
          Ok (List.rev acc)
      | hd :: tl ->
          match f hd with
          | Ok v -> loop (v :: acc) tl
          | (Error _) as err -> err
    in
    loop [] l

  module Syntax = struct
    let (let*) = bind

    let (let+) x f = map f x
  end
end

module Hashtbl = struct
  module type HashedType = sig
    include MoreLabels.Hashtbl.HashedType

    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end

  module type S = sig
    include MoreLabels.Hashtbl.S

    val filter : f:(key:key -> data:'a -> bool) -> 'a t -> 'a t
    val random_opt : 'a t -> (key * 'a) option
    val random_exn : 'a t -> key * 'a
    val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
    val of_yojson : (Yojson.Safe.t -> ('a, string) result) -> Yojson.Safe.t -> ('a t, string) result
  end

  module Make (H : HashedType) : S with type key = H.t = struct
    include MoreLabels.Hashtbl.Make (H)

    type 'value key_value = { key : H.t; value : 'value } [@@deriving yojson]

    let filter ~f tbl =
      let tbl' = create @@ length tbl in
      iter tbl ~f:(fun ~key ~data ->
        match f ~key ~data with
        | true -> add tbl' ~key ~data
        | false -> ()
      );
      tbl'

    let random_opt tbl =
      match length tbl with
      | 0 ->
          None
      | n ->
          let chosen = Random.int n in
          let rec loop_until counter seq =
            match counter, seq () with
            | _, Seq.Nil -> None
            | 0, Seq.Cons (elt, _) -> Some elt
            | _, Seq.Cons (_, seq) -> loop_until (counter - 1) seq
          in
          loop_until chosen (to_seq tbl)

    let random_exn tbl =
      match random_opt tbl with
      | Some elt -> elt
      | None -> raise Not_found

    let to_yojson value_to_yojson tbl =
      tbl
      |> to_seq
      |> Seq.map (fun (key, value) -> { key; value })
      |> List.of_seq
      |> [%derive.to_yojson: 'a key_value list] value_to_yojson

    let of_yojson value_of_yojson yojson =
      yojson
      |> [%derive.of_yojson: 'a key_value list] value_of_yojson
      |> Result.map (fun l -> l |> List.to_seq |> Seq.map (fun { key; value } -> (key, value)) |> of_seq)
  end
end

module Map = MoreLabels.Map

module Set = struct
  module type OrderedType = MoreLabels.Set.OrderedType

  module type S = sig
    include MoreLabels.Set.S

    val random_opt : t -> elt option
    val random_exn : t -> elt
  end

  module Make (Ord : OrderedType) : S with type elt = Ord.t = struct
    include MoreLabels.Set.Make (Ord)

    let random_opt set =
      match cardinal set with
      | 0 ->
          None
      | n ->
          let chosen = Random.int n in
          let rec loop_until counter seq =
            match counter, seq () with
            | _, Seq.Nil -> None
            | 0, Seq.Cons (elt, _) -> Some elt
            | _, Seq.Cons (_, seq) -> loop_until (counter - 1) seq
          in
          loop_until chosen (to_seq set)

    let random_exn set =
      match random_opt set with
      | Some elt -> elt
      | None -> raise Not_found
  end
end
