module ArrayLabels = struct
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
end

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
