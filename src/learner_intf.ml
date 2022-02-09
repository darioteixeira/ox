module type S = sig
  module Action : Action_intf.S

  type state [@@deriving repr]
  (** The current state of the XCS learner. *)

  val create : Config.t -> state
  (** Create a new learner using the given {!Config}. *)

  val provide_environment : state -> bool array -> Action.t
  (** [provide_environment state environment] feeds the current environment to the learner,
      returning the learner's recommended {!Action}. Note that the learner's state is modified! *)

  val provide_feedback : reward:float -> is_final_step:bool -> state -> unit
  (** [provide_feedback ~reward ~is_final_step state] provides the learner with feedback
      (in the form of a reward) concerning the {!Action} it recommended in the previous
      invocation of{!provide_environment}. For multi-step problems, [is_final_step] indicates
      whether this is the final step. For single-step problems, [is_final_step] is always [true].
      Note that the learner's state is modified! *)
end
