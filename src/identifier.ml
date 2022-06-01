type t = string [@@deriving yojson]

let equal = String.equal
let hash = Hashtbl.hash
let of_string = Fun.id
let pp = Format.pp_print_string
