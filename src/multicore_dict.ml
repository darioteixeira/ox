module Array = ArrayLabels
module Task = Domainslib.Task

include Multicore_dict_intf

module Make (C : MULTICORE_CONFIG) (H : Dict.HASHABLE) : Dict.IMPL with type key = H.t = struct
  module Monotbl = MoreLabels.Hashtbl.Make (H)

  type key = H.t [@@deriving yojson]

  type 'a t = 'a Monotbl.t array

  let bucket_idx_of_key key =
    H.hash key mod C.num_domains

  let length buckets =
    Array.fold_left ~init:0 ~f:(fun acc tbl -> acc + Monotbl.length tbl) buckets

  let create size =
    Array.init C.num_domains ~f:(fun _idx -> Monotbl.create size)

  let add buckets ~key ~data =
    let idx = bucket_idx_of_key key in
    Monotbl.add ~key ~data buckets.( idx )

  let remove buckets key =
    let idx = bucket_idx_of_key key in
    Monotbl.remove buckets.( idx ) key

  let find_opt buckets key =
    let idx = bucket_idx_of_key key in
    Monotbl.find_opt buckets.( idx ) key

  let mem buckets key =
    let idx = bucket_idx_of_key key in
    Monotbl.mem buckets.( idx ) key

  let iter ~f buckets =
    Array.iter buckets ~f:(Monotbl.iter ~f)

  let filter ~f buckets =
    let buckets' = create @@ length buckets in
    Array.iteri buckets ~f:(fun idx tbl ->
      let tbl' = buckets'.( idx ) in
      Monotbl.iter tbl ~f:(fun ~key ~data ->
        match f ~key ~data with
        | true -> Monotbl.add tbl' ~key ~data
        | false -> ()
      )
    );
    buckets'

  let filter_map_inplace ~f buckets =
    Array.iter buckets ~f:(Monotbl.filter_map_inplace ~f)

  let fold ~f buckets ~init =
    Array.fold_left buckets ~init ~f:(fun acc tbl -> Monotbl.fold ~init:acc ~f tbl)

  let parallel_fold ~init ~neutral ~combine ~f buckets =
    Task.run C.task_pool @@ fun () ->
      Task.parallel_for_reduce
        ~chunk_size:1
        ~start:0
        ~finish:(C.num_domains - 1)
        ~body:(fun idx -> Monotbl.fold ~init:neutral ~f buckets.( idx ))
        C.task_pool
        combine
        init

  let to_seq buckets =
    let rec f (idx, seq) =
      match Seq.uncons seq with
      | Some (elt, seq') -> Some (elt, (idx, seq'))
      | None when idx < Array.length buckets - 1 -> f (idx + 1, Monotbl.to_seq buckets.( idx + 1 ))
      | None -> None
    in
    Seq.unfold f (0, Monotbl.to_seq buckets.( 0 ))

  let of_seq seq =
    let buckets = create 16 in
    Seq.iter (fun (key, data) -> add ~key ~data buckets) seq;
    buckets
end
