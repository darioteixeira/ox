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
