include module type of Oxen_intf

module Make (Action : Action_intf.S) : S with module Action = Action
