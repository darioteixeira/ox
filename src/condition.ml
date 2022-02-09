module Array = ArrayLabels
module String = StringLabels

type trilean =
  | True
  | False
  | Wildcard
  [@@deriving repr]

type environment = bool array

type t = {
  trileans : trilean array;
  genericity : int; (* Cached computed value: number of wildcards in the array of trileans. *)
} [@@deriving repr]

let char_of_trilean = function
  | True -> '1'
  | False -> '0'
  | Wildcard -> '#'

let trilean_of_boolean = function
  | true -> True
  | false -> False

let compute_genericity trileans =
  Array.fold_left trileans ~init:0 ~f:(fun acc trilean ->
    match trilean with
    | Wildcard -> acc + 1
    | _ -> acc
  )

let to_string { trileans; _ } =
  String.init (Array.length trileans) ~f:(fun idx -> char_of_trilean trileans.( idx ))

let genericity { genericity; _ } =
  genericity

let matches { trileans; _ } booleans =
  let trilean_matches trilean boolean =
    match (trilean, boolean) with
    | (Wildcard, _) -> true
    | (True, true) -> true
    | (False, false) -> true
    | (True, false) -> false
    | (False, true) -> false
  in
  Array.for_all2 ~f:trilean_matches trileans booleans

let is_more_general ~than subject =
  subject.genericity > than.genericity
  && Array.for_all2 subject.trileans than.trileans ~f:(fun e1 e2 ->
    match e1, e2 with
    | Wildcard, _ -> true
    | _, Wildcard -> true
    | e1, e2 when e1 = e2 -> true
    | _ -> false
  )

let make_from_environment ~wildcard_probability environment =
  let (genericity, trileans) =
    Array.fold_left_map environment ~init:0 ~f:(fun acc b ->
      if Random.float 1.0 < wildcard_probability
      then (acc + 1, Wildcard)
      else (acc, trilean_of_boolean b)
    )
  in
  { trileans; genericity }

let maybe_mutate_trilean ~mutation_probability trilean environment =
  match Random.float 1. < mutation_probability, trilean with
  | false, trilean -> trilean
  | true, Wildcard -> trilean_of_boolean environment
  | true, (True | False) -> Wildcard

let clone_with_mutation ~mutation_probability { trileans; _ } environment =
  let trileans = Array.mapi trileans ~f:(fun i trilean -> maybe_mutate_trilean ~mutation_probability trilean environment.( i )) in
  let genericity = compute_genericity trileans in
  { trileans; genericity }

let crossover_with_mutation ~mutation_probability condition1 condition2 environment =
  let { trileans = trileans1; _ } = condition1 in
  let { trileans = trileans2; _ } = condition2 in
  let len = Array.length trileans1 in
  let apply_single_point_crossover trileans_a trileans_b =
    let t1 = maybe_mutate_trilean ~mutation_probability trileans_a.( 0 ) environment.( 0 ) in
    let t2 = maybe_mutate_trilean ~mutation_probability trileans_b.( 1 ) environment.( 1 ) in
    let trileans = [| t1; t2 |] in
    let genericity = compute_genericity trileans in
    { trileans; genericity }
  in
  let apply_two_point_crossover ~low ~high trileans_a trileans_b =
    let trileans =
      Array.init len ~f:(fun idx ->
        let src = if idx <= low || idx > high then trileans_a else trileans_b in
        maybe_mutate_trilean ~mutation_probability src.( idx ) environment.( idx )
      )
    in
    let genericity = compute_genericity trileans in
    { trileans; genericity }
  in
  match len with
  | 0 | 1 ->
      (* These are degenerate cases, and we can do whatever we want with them. *)
      (condition1, condition2)
  | 2 ->
      (* Single-point crossover with mutation. *)
      let c1 = apply_single_point_crossover trileans1 trileans2 in
      let c2 = apply_single_point_crossover trileans2 trileans1 in
      (c1, c2)
  | length ->
      (* Proper two-point crossover with random crossover points. *)
      let low = Random.int (length - 2) in
      let high = 1 + Random.int (length - 2) in
      let (low, high) =
        match Int.compare low high with
        | -1 -> (low, high)
        | 1 -> (high, low)
        | _ -> (low - 1, high + 1)
      in
      let c1 = apply_two_point_crossover ~low ~high trileans1 trileans2 in
      let c2 = apply_two_point_crossover ~low ~high trileans2 trileans1 in
      (c1, c2)
