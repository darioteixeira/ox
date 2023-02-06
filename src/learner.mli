include module type of Learner_intf

module Make_singlecore (Sensors_def: Sensors.DEF) (Action : Action.S) : S with type sensors = Sensors_def.sensors and type action = Action.t

module Make_multicore (Sensors_def: Sensors.DEF) (Action : Action.S) (MConfig : Multicore_config.S) : S with type sensors = Sensors_def.sensors and type action = Action.t
