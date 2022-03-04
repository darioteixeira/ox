module type S = sig
  type sensors
  (** A condition is parameterised by the type of its sensors. *)

  type t
  (** A condition is essentially a representation of {!Environment.t}
      where each individual locus may be replaced by a wildcard symbol.
      A single condition may therefore match multiple environments. *)

  val make_from_environment : wildcard_probability:float -> sensors Environment.t -> t
  (** Create a condition using the provided environment as template.
      With [wildcard_probability], each locus from the environment
      may be transformed into a wildcard symbol. *)

  val genericity : t -> int
  (** Returns the number of wildcards in the given condition. *)

  val matches : t -> sensors Environment.t -> bool
  (** Does the given condition match the given environment? *)

  val is_more_general : than:t -> t -> bool
  (** Is the given condition more general than the given reference condition?
      A condition is more general if it has higher genericity (i.e. a higher
      number of wildcards) and the conditions have identical loci in all
      positions where neither has a wildcard. *)

  val clone_with_mutation :
    mutation_probability:float ->
    wildcard_probability:float ->
    ?environment:sensors Environment.t ->
    t ->
    t
  (** [clone_with_mutation ~mutation_probability ~wildcard_probability ?environment condition]
      returns a new condition based on the given [condition]. With [mutation_probability],
      each locus from the condition will be mutated; if the old locus is not a wildcard,
      then it will be changed into a wildcard with [wildcard_probability]. In addition,
      if [environment] is also given, then all mutations that arise in the new condition
      are guaranteed to match the environment. *)

  val crossover_with_mutation :
    mutation_probability:float ->
    wildcard_probability:float ->
    ?environment:sensors Environment.t ->
    t ->
    t ->
    t * t
  (** [crossover_with_mutation ~mutation_probability ~wildcard_probability ?environment condition1 condition2]
      performs a uniform crossover of the two given conditions, returning both a new crossed over condition
      and its inverse counterpart. *)

  val to_string : ?intra_group_separator:char -> ?inter_group_separator:char -> t -> string
  (** Serialises a condition into a string. For a correct round-trip invocation
      of [to_string] followed by {!of_string}, the [intra_group_separator] and
      [inter_group_separator] parameters must be identical (or equally absent)
      in both invocations. Note that [inter_group_separator] defaults to a
      semicolon character, whereas [intra_group_separator] is absent unless
      explicitly provided.  Moreover, if any of the sensor values is serialised
      into a multi-character string, then correct deserialisation with {!of_string}
      can only occurr if [intra_group_separator] is explicitly provided. Finally,
      a safe round-trip serialisation/deserialisation also requires that the
      characters used in the [intra_group_separator] and [inter_group_separator]
      are not used in the string representation of any sensor values.
      *)

  val of_string : ?intra_group_separator:char -> ?inter_group_separator:char -> string -> t
  (** Deserialises a condition from a string.
      See {!to_string} for details concerning safe usage. *)

  val to_yojson : t -> Yojson.Safe.t
  (** JSON serialisation. *)

  val of_yojson : Yojson.Safe.t -> (t, string) result
  (** JSON deserialisation. *)

  val equal : t -> t -> bool
  (** Standard equality function. *)
end
