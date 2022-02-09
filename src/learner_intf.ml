module type S = sig
  module Action : Action_intf.S

  type t [@@deriving repr]
  (** Represents the current state of a XCS learner. *)

  val create : Config.t -> t
  (** Create a new learner using the given {!Config}. *)

  val provide_environment : t -> bool array -> Action.t
  (** [provide_environment learner environment] feeds the current environment to the learner,
      returning the learner's recommended {!Action}. Note that the learner's internal state
      is modified! *)

  val provide_feedback : reward:float -> is_final_step:bool -> t -> unit
  (** [provide_feedback ~reward ~is_final_step learner] provides the learner with feedback
      (in the form of a reward) concerning the {!Action} it recommended in the previous
      invocation of{!provide_environment}. For multi-step problems, [is_final_step] indicates
      whether this is the final step. For single-step problems, [is_final_step] is always [true].
      Note that the learner's internal state is modified! *)
end