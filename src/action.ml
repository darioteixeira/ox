module type S = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val all : t list
  val to_string : t -> string
  val of_string : string -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end
