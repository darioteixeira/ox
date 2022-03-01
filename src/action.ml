include Action_intf

module Make_jsonable (A : S) : JSONABLE with type t = A.t = struct
  include A

  let to_yojson v = `String (to_string v)

  let of_yojson = function
    | `String str -> Ok (of_string str)
    | _ -> Error "Action.of_yojson"
end
