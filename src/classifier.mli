include module type of Classifier_intf

module Make (Condition : Condition.S) (Action : Action.S) : S with type condition = Condition.t and type action = Action.t
