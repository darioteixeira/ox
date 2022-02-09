(** This is the signature of the module that parameterises the {!Learner.Make} functor.
    It defines the space of possible actions available to the learner. *)
module type S = sig
  type t [@@deriving repr]

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val all : t list
  val to_string : t -> string
end
