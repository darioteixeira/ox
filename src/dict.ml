include Dict_intf

module Make (Impl : IMPL) : S
  with type key = Impl.key
  and type 'a t = 'a Impl.t = struct

  include Impl

  type 'value key_value = { key : key; value : 'value } [@@deriving yojson]

  let random_opt dict =
    match length dict with
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
        loop_until chosen (to_seq dict)

  let random_exn dict =
    match random_opt dict with
    | Some elt -> elt
    | None -> raise Not_found

  let to_yojson value_to_yojson dict =
    dict
    |> to_seq
    |> Seq.map (fun (key, value) -> { key; value })
    |> List.of_seq
    |> [%derive.to_yojson: 'a key_value list] value_to_yojson

  let of_yojson value_of_yojson yojson =
    yojson
    |> [%derive.of_yojson: 'a key_value list] value_of_yojson
    |> Result.map (fun l -> l |> List.to_seq |> Seq.map (fun { key; value } -> (key, value)) |> of_seq)
end
