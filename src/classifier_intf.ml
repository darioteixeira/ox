module type S = sig
  type condition
  type action

  type t = private {
    identifier : Identifier.t;
    (** This identifier is computed from the [condition] and [action]. *)
    condition : condition;
    (** Environmental state(s) which the classifier matches. *)
    action : action;
    (** Action proposed by the classifier upon a match. *)
    mutable prediction : float;
    (** Parameter [p]: Estimates the expected reward if the action is taken. *)
    mutable prediction_error : float;
    (** Parameter [ε]: Estimates the errors made in predictions. *)
    mutable fitness : float;
    (** Parameter [F]: Fitness of the classifier. *)
    mutable last_occurrence : int;
    (** Parameter [ts]: Time-step of the last run of a genetic algorithm in an action set this classifier belonged to. *)
    mutable experience : int;
    (** Parameter [exp]: Counter of the number times that the classifier has belonged to an action set. *)
    mutable avg_action_set_size : float;
    (** Parameter [as]: Average size of the action sets this classifier has belonged to. *)
    mutable numerosity : int;
    (** Parameter [n]: Number of micro-classifiers this classifier represents. *)
    mutable accuracy : float;
    (** Parameter [k]: Accuracy of the classifier. Note that this is a cached computed quantity based on the prediction error [ε]. *)
    mutable weight : float;
    (** Several routines attach a routine-specific weight to a classifier.
        Storing this weight as a classifier field saves us from constantly
        allocating ancillary data structures.
    *)
  } [@@deriving show, yojson]

  val make :
    condition:condition ->
    action:action ->
    prediction:float ->
    prediction_error:float ->
    fitness:float ->
    last_occurrence:int ->
    experience:int ->
    avg_action_set_size:float ->
    numerosity:int ->
    accuracy:float ->
    t
  (** Creates a fresh classifier with the provided parameters. *)

  val update :
    ?prediction:float ->
    ?prediction_error:float ->
    ?fitness:float ->
    ?last_occurrence:int ->
    ?experience:int ->
    ?avg_action_set_size:float ->
    ?numerosity:int ->
    ?accuracy:float ->
    t ->
    unit
  (** Updates the given classifier in-place. *)

  val clone :
    ?condition:condition ->
    ?action:action ->
    ?prediction:float ->
    ?prediction_error:float ->
    ?fitness:float ->
    ?last_occurrence:int ->
    ?experience:int ->
    ?avg_action_set_size:float ->
    ?numerosity:int ->
    ?accuracy:float ->
    t ->
    t
  (** Returns a fresh classifier using the provided classifier as template. *)

  val equal : t -> t -> bool
  val identifier : t -> Identifier.t
  val fitness : t -> float
  val set_weight : t -> float -> unit
end
