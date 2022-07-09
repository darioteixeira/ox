include module type of Learner_intf

module Make (Sensors_def: Sensors.DEF) (Action : Action.S) (Maketbl : Dict.MAKETBL) : S with type sensors = Sensors_def.sensors and type action = Action.t
