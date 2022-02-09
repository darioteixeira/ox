module type S = sig
  type t [@@deriving repr]

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val all : t list
  val to_string : t -> string
end
