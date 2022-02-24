(** Signature of a learner based on the XCS learning classifier system.
    Usage of this API always starts by creating a new learner via the
    {!S.create} function. Afterwards, and until some termination condition
    is satisfied, one successively invokes the {!S.iterate} function. *)
module type S = sig
  module Sensors_def : Sensors.DEF
  module Action : Action.S

  type t
  (** An XCS learner. *)

  val logs_src : Logs.src
  (** You can use this to control the logging behaviour of the learner.
      (Please consult the documentation of the library for details.) *)

  val create : config:Config.t -> t
  (** Create a new learner using the given {!Config.t}. *)

  val update_config : config:Config.t -> t -> unit
  (** Updates the configuration of the given learner. *)

  val iterate : t -> Sensors_def.sensors Environment.t -> (Action.t -> float * bool) -> unit
  (** [iterate learner environment handle_action] starts by feeding the environment to the learner,
      which responds with the action it deems appropriate. This action is then given to the function
      [handle_action], which must return a pair consisting of a suitable reward and a boolean
      indicating whether this was the final step in a multi-step problem (for single-step problems,
      this boolean should always be true). *)
end
