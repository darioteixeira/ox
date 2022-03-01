include module type of Action_intf

module Make_jsonable (A : S) : JSONABLE with type t = A.t
