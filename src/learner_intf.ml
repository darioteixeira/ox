(** Signature of a learner based on the XCS learning classifier system.
    Usage of this API always starts by creating a new learner via the
    {!S.create} function. Afterwards, and until some termination condition
    is satisfied, one must alternate invocations of {!S.provide_environment}
    and {!S.provide_feedback}. The exceptions {!S.Expected_environment}
    and {!S.Expected_feedback} will be raised otherwise. *)
module type S = sig
  module Action : Action_intf.S

  type t [@@deriving repr]
  (** Represents the current state of a XCS learner. *)

  exception Expected_environment
  (** Raised if {!provide_feedback} is invoked on a learner which is expecting
      an invocation of {!provide_environment} instead. *)

  exception Expected_feedback
  (** Raised if {!provide_environment} is invoked on a learner which is expecting
      an invocation of {!provide_feedback} instead. *)

  val logs_src : Logs.src
  (** You can use this to control the logging behaviour of the learner.
      (Please consult the documentation of the library for details.) *)

  val create : Config.t -> t
  (** Create a new learner using the given {!Config}. *)

  val provide_environment : t -> bool array -> Action.t
  (** [provide_environment learner environment] feeds the current environment to the learner,
      returning the learner's recommended {!Action}. Note that the learner's internal state
      is modified! (Raises {!Expected_feedback} if the learner expects an invocation of
      {!provide_feedback} instead.) *)

  val provide_feedback : reward:float -> is_final_step:bool -> t -> unit
  (** [provide_feedback ~reward ~is_final_step learner] provides the learner with feedback
      (in the form of a reward) concerning the {!Action} it recommended in the previous
      invocation of{!provide_environment}. For multi-step problems, [is_final_step] indicates
      whether this is the final step. For single-step problems, [is_final_step] is always [true].
      Note that the learner's internal state is modified! (Raises {!Expected_environment}
      if the learner expects an invocation of {!provide_environment} instead.)*)
end
