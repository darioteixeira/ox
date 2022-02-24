module type S = sig
  (** A condition is parameterised by the type of its sensors. *)
  module Sensors_def : Sensors.DEF

  (** A condition is essentially a representation of {!Environment.t}
      where each individual element may be replaced by a {i wildcard} symbol.
      A single condition may therefore match multiple environments. *)
  type t

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
      are not used in the string representation of any of sensors values.
      *)
  val to_string : ?intra_group_separator:char -> ?inter_group_separator:char -> t -> string

  (** Deserialises a condition from a string.
      See {!to_string} for details concerning safe usage. *)
  val of_string : ?intra_group_separator:char -> ?inter_group_separator:char -> string -> t

  (** Returns the number of wildcards in the given condition. *)
  val genericity : t -> int

  (** Does the given condition match the given environment? *)
  val matches : t -> Sensors_def.sensors Environment.t -> bool

  (** Is the given condition more general than the given reference condition?
      A condition is more general if it has higher genericity (i.e. a higher
      number of wildcards) and the conditions have identical elements in all
      positions where neither has a wildcard. *)
  val is_more_general : than:t -> t -> bool

  (** Create a condition using the provided sensors and environment as template.
      With [wildcard_probability], each element from the environment may be
      transformed into a wildcard symbol. *)
  val make_from_environment : wildcard_probability:float -> Sensors_def.sensors Environment.t -> t

  (** [clone_with_mutation ~mutation_probability ~wildcard_probability ?environment condition]
      returns a new condition based on the given [condition]. With [mutation_probability],
      each element from the condition will be mutated; if the old element is not a wildcard,
      then it will be changed into a wildcard with [wildcard_probability]. In addition,
      if [environment] is also given, then all mutations that arise in the new condition
      are guaranteed to match the environment. *)
  val clone_with_mutation :
    mutation_probability:float ->
    wildcard_probability:float ->
    ?environment:Sensors_def.sensors Environment.t ->
    t ->
    t

  (** [crossover_with_mutation ~mutation_probability ~wildcard_probability ?environment condition1 condition2]
      performs a uniform crossover of the two given conditions, returning both a new crossed over condition
      and its inverse counterpart. *)
  val crossover_with_mutation :
    mutation_probability:float ->
    wildcard_probability:float ->
    ?environment:Sensors_def.sensors Environment.t ->
    t ->
    t ->
    t * t
end
