include module type of Condition_intf

module Make (Sensors_def : Sensors.DEF) : S with type sensors = Sensors_def.sensors
