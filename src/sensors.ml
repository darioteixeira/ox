module type SENSOR = sig
  type t

  val equal : t -> t -> bool
  val random : ?exclude:t -> unit -> t
  val to_string : t -> string
  val of_string : string -> t
end

type 'a sensor = (module SENSOR with type t = 'a)

type _ t =
  | [] : unit t
  | ( :: ) : 'a sensor * 'b t -> ('a array * 'b) t

module type DEF = sig
  type sensors

  val sensors : sensors t
end

module Binary = struct
  type t = bool

  let equal = Bool.equal

  let random ?exclude () =
    match exclude with
    | Some true -> false
    | Some false -> true
    | None -> Random.bool ()

  let to_string = function
    | false -> "0"
    | true -> "1"

  let of_string = function
    | "0" -> false
    | "1" -> true
    | str -> invalid_arg ("Sensors.Binary.of_string: " ^ str)
end

module Ternary = struct
  type t = False | True | Unknown

  let equal a b =
    match a, b with
    | False, False
    | True, True
    | Unknown, Unknown -> true
    | _ -> false

  let random ?exclude () =
    match exclude with
    | Some False when Random.bool () -> True
    | Some False -> Unknown
    | Some True when Random.bool () -> Unknown
    | Some True -> False
    | Some Unknown when Random.bool () -> False
    | Some Unknown -> True
    | None ->
        match Random.int 3 with
        | 0 -> False
        | 1 -> True
        | _ -> Unknown

  let to_string = function
    | False -> "0"
    | True -> "1"
    | Unknown -> "_"

  let of_string = function
    | "0" -> False
    | "1" -> True
    | "_" -> Unknown
    | str -> invalid_arg ("Sensors.Ternary.of_string: " ^ str)
end

let binary = (module Binary : SENSOR with type t = Binary.t)

let ternary = (module Ternary : SENSOR with type t = Ternary.t)
