module type SENSOR = sig
  type t

  val equal : t -> t -> bool
  val random : ?exclude:t -> unit -> t
  val to_string : t -> string
  val of_string : string -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

type 'a sensor = (module SENSOR with type t = 'a)

type _ t =
  | [] : unit t
  | ( :: ) : 'a sensor * 'b t -> ('a array -> 'b) t

module type DEF = sig
  type sensors

  val sensors : sensors t
end

module Binary : sig
  type t = bool

  include SENSOR with type t := t
end

module Ternary : sig
  type t = False | True | Unknown

  include SENSOR with type t := t
end

val binary : Binary.t sensor

val ternary : Ternary.t sensor
