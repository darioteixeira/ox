(** The type of ternary logic "booleans". *)
type trilean = True | False | Wildcard [@@deriving repr]

(** The environment is encoded into an array of booleans. *)
type environment = bool array

(** A [condition] {!t} is essentially a {!trilean} vector. *)
type t [@@deriving repr]

val of_array : trilean array -> t
(** Creates a {!t} directly from the given array of trileans. *)

val to_string : t -> string
(** Returns a string representation of the given condition. *)

val genericity : t -> int
(** Returns the number of wildcards in the given condition. *)

val matches : t -> environment -> bool
(** Does the given condition satisfy the given {!environment}? *)

val is_more_general : than:t -> t -> bool
(** Is the given condition more general than the given reference condition?
    A condition is more general if it has higher genericity (i.e. a higher
    number of wildcards) and the conditions have identical elements in all
    positions where neither has a wildcard. *)

val make_from_environment : wildcard_probability:float -> environment -> t
(** Create a {!t} using the provided {!environment} as template.
    With [wildcard_probability], each boolean from the template
    may be transformed into a wildcard symbol. *)

val clone_with_mutation : mutation_probability:float -> t -> environment -> t
(** [clone_with_mutation ~mutation_probability condition environment] returns a new {!t}
    based on the given [condition], where each element is changed with [mutation_probability]
    in such a way that the new mutated condition still matches [environment]. *)

val crossover_with_mutation : mutation_probability:float -> t -> t -> environment -> t * t
(** [crossover_with_mutation] performs a two-point crossover of the two given conditions,
    returning both a new crossed over condition and its inverse counterpart. Furthermore,
    each element is changed with [mutation_probability] in such a way that the resulting
    conditions still match [environment]. The function also handles the edge cases when
    the length of the conditions is too small to apply a randomised two-point crossover,
    though in practice these should never actually happen with real-life problems. *)
