include module type of Classifier_intf

module Make (Condition : Condition.S) (Action : Action.S) : S with module Condition = Condition and module Action = Action
