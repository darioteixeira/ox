module Make (H : Dict.HASHABLE) : Dict.IMPL with type key = H.t = struct

  include MoreLabels.Hashtbl.Make (H)

  let key_to_yojson = H.to_yojson

  let key_of_yojson = H.of_yojson

  let filter ~f tbl =
    let tbl' = create @@ length tbl in
    iter tbl ~f:(fun ~key ~data ->
      match f ~key ~data with
      | true -> add tbl' ~key ~data
      | false -> ()
    );
    tbl'

  let parallel_fold ~init ~neutral:_ ~combine:_ ~f tbl =
    fold ~init ~f tbl
end
