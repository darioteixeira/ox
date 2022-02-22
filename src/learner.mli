include module type of Learner_intf

module Make (Sensors_def: Sensors.DEF) (Action : Action.S) : S with module Sensors_def = Sensors_def and module Action = Action
