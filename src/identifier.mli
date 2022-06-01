type t = private string [@@deriving yojson]

val equal : t -> t -> bool
val hash : t -> int
val of_string : string -> t
val pp : Format.formatter -> t -> unit
