include module type of Classifier_intf

module Make (Action : Action_intf.S) : S with module Action = Action
