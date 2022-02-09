include module type of Learner_intf

module Make (Action : Action_intf.S) : S with module Action = Action
