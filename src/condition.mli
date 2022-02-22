include module type of Condition_intf

module Make (Sensors_def : Sensors.DEF) : S with module Sensors_def = Sensors_def
