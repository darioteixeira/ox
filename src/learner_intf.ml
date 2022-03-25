(** Signature of a learner based on the XCS learning classifier system.
    Usage of this API always starts by creating a new learner via the
    {!S.create} function. Afterwards, and until some termination condition
    is satisfied, one successively invokes the {!S.iterate} function. *)
module type S = sig
  type sensors

  type action

  type t
  (** An XCS learner. *)

  val logs_src : Logs.src
  (** You can use this to control the logging behaviour of the learner.
      (Please consult the documentation of the library for details.) *)

  val create : config:Config.t -> t
  (** Create a new learner using the given {!Config.t}. *)

  val get_config : t -> Config.t
  (** Returns the current configuration of the given learner. *)

  val update_config : config:Config.t -> t -> unit
  (** Updates the configuration of the given learner. *)

  val iterate : t -> sensors Environment.t -> (action -> float * bool) -> action * float * bool
  (** [iterate learner environment handle_action] starts by feeding the environment to the learner,
      which responds with the action it deems appropriate. This action is then given to the function
      [handle_action], which must return a pair consisting of a suitable reward and a boolean
      indicating whether this was the final step in a multi-step problem (for single-step problems,
      this boolean should always be true). The function returns a triple consisting of the action
      provided to [handle_action] together with the reward and the boolean produced by the latter. *)

  (** {1 Low-level interface } *)

  exception Expected_environment
  (** Raised if {!provide_feedback} is invoked on a learner which is expecting
      an invocation of {!provide_environment} instead. *)

  exception Expected_feedback
  (** Raised if {!provide_environment} is invoked on a learner which is expecting
      an invocation of {!provide_feedback} instead. *)

  val provide_environment : t -> sensors Environment.t -> action
  (** [provide_environment learner environment] feeds the current environment to the learner,
      returning the learner's recommended {!Action}. Note that the learner's internal state
      is modified. (Raises {!Expected_feedback} if the learner expects an invocation of
      {!provide_feedback} instead.) *)

  val provide_feedback : reward:float -> is_final_step:bool -> t -> unit
  (** [provide_feedback ~reward ~is_final_step learner] provides the learner with feedback
      (in the form of a reward) concerning the {!Action} it recommended in the previous
      invocation of{!provide_environment}. For multi-step problems, [is_final_step] indicates
      whether this is the final step. For single-step problems, [is_final_step] is always [true].
      Note that the learner's internal state is modified. (Raises {!Expected_environment}
      if the learner expects an invocation of {!provide_environment} instead.)*)

  (** {1 JSON (de)serialisation } *)

  val to_yojson : t -> Yojson.Safe.t
  (** JSON serialisation. *)

  val of_yojson : Yojson.Safe.t -> (t, string) result
  (** JSON deserialisation. *)
end
