(** Signature of a learner based on the XCS learning classifier system.
    Usage of this API always starts by creating a new learner via the
    {!S.create} function. The API is imperative; the core functions
    ({!S.provide_environment}, {!S.provide_intermediate_feedback},
    and {!S.provide_final_feedback} modify the given learner).
    Until some termination condition is satisfied, the typical sequence
    of function calls depends on whether we are dealing with a single-step
    versus a multi-sep problem.

    For single-step problems (e.g. classification problems):
    {v
    S.create
    while not termination condition:
      S.provide_environment
      S.provide_final_feedback
    v}

    For multi-step problems (e.g. game of Tic-Tac-Toe):
    {v
    S.create
    while not termination condition:
      repeat until final step:
        S.provide_environment
        S.provide_intermediate_feedback
      S.provide_final_feedback
    v}
*)
module type S = sig
  type sensors

  type action

  type t
  (** An XCS learner. *)

  type stats = {
    population_size : int;
    population_numerosity : int;
  }
  (** Learner statistics as provided by {!get_stats}. *)

  exception Wrong_state
  (** Raised if invocations of {!provide_environment}, {!provide_intermediate_feedback},
      and {!provide_final_feedback} are not sequenced in accordance to valid transitions
      for the Learner's state machine. *)

  val logs_src : Logs.src
  (** You can use this to control the logging behaviour of the learner.
      (Please consult the documentation of the library for details.) *)

  val create : config:Config.t -> t
  (** Create a new learner using the given {!Config.t}. *)

  val get_stats : t -> stats
  (** Returns basic statistics regarding the learner. *)

  val get_config : t -> Config.t
  (** Returns the current configuration of the given learner. *)

  val update_config : config:Config.t -> t -> unit
  (** Updates the configuration of the given learner. *)

  val provide_environment : t -> sensors Environment.t -> action
  (** [provide_environment learner environment] feeds the current environment to the learner,
      returning the learner's recommended {!Action}. Note that the learner's internal state is
      modified. (Raises {!Wrong_state} if {!provide_environment} is invoked twice in a row without
      any interleaved calls to {!provide_intermediate_feedback} or {!provide_final_feedback}.) *)

  val provide_intermediate_feedback : reward:float -> t -> unit
  (** [provide_intermediate_feedback ~reward learner] provides the learner with intermediate
      feedback in the form of a reward concerning the {!Action} it recommended in the previous
      invocation of {!provide_environment}. This function is only relevant for multi-step problems.
      Note that the learner's internal state is modified. (Raises {!Wrong_state} if invoked right
      after another invocation of {!provide_intermediate_feedback} or {!provide_final_feedback}.) *)

  val provide_final_feedback : reward:float -> t -> unit
  (** [provide_final_feedback ~reward learner] provides the learner with final feedback in the form
      of a reward concerning the {!Action} it recommended in the previous invocation of {!provide_environment}.
      For single-step problems, this is the only relevant function for providing feedback, and must always
      be invoked interlaced with {!provide_environment}. For multi-step problems, this function may be invoked
      after {!provide_environment} or {!provide_intermediate_feedback}.  Note that the learner's internal state
      is modified. (Raises {!Wrong_state} if not preceded by an invocation of {!provide_environment} or
      {!provide_intermediate_feedback}.) *)

  (** {1 JSON (de)serialisation } *)

  val to_yojson : t -> Yojson.Safe.t
  (** JSON serialisation. *)

  val of_yojson : Yojson.Safe.t -> (t, string) result
  (** JSON deserialisation. *)
end
