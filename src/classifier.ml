include Classifier_intf

module Make (Condition : Condition.S) (Action : Action.S) : S with type condition = Condition.t and type action = Action.t = struct

  type condition = Condition.t [@@deriving yojson]
  type action = Action.t [@@deriving yojson]

  type t = {
    identifier : Identifier.t;
    condition : condition;
    action : action;
    mutable prediction : float;
    mutable prediction_error : float;
    mutable fitness : float;
    mutable last_occurrence : int;
    mutable experience : int;
    mutable avg_action_set_size : float;
    mutable numerosity : int;
    mutable accuracy : float;
  } [@@deriving yojson]

  let make_identifier ~condition ~action =
    Identifier.of_string @@ Printf.sprintf "%s-%s" (Condition.to_string condition) (Action.to_string action)

  let make
    ~condition ~action ~prediction ~prediction_error ~fitness ~last_occurrence
    ~experience ~avg_action_set_size ~numerosity ~accuracy =
      let identifier = make_identifier ~condition ~action in
      {
        identifier;
        condition;
        action;
        prediction;
        prediction_error;
        fitness;
        last_occurrence;
        experience;
        avg_action_set_size;
        numerosity;
        accuracy;
      }

  let update
    ?prediction ?prediction_error ?fitness ?last_occurrence
    ?experience ?avg_action_set_size ?numerosity ?accuracy
    classifier =
      classifier.prediction <- Option.value ~default:classifier.prediction prediction;
      classifier.prediction_error <- Option.value ~default:classifier.prediction_error prediction_error;
      classifier.fitness <- Option.value ~default:classifier.fitness fitness;
      classifier.last_occurrence <- Option.value ~default:classifier.last_occurrence last_occurrence;
      classifier.experience <- Option.value ~default:classifier.experience experience;
      classifier.avg_action_set_size <- Option.value ~default:classifier.avg_action_set_size avg_action_set_size;
      classifier.numerosity <- Option.value ~default:classifier.numerosity numerosity;
      classifier.accuracy <- Option.value ~default:classifier.accuracy accuracy

  let clone
    ?condition ?action ?prediction ?prediction_error ?fitness ?last_occurrence
    ?experience ?avg_action_set_size ?numerosity ?accuracy
    classifier =
      let condition = Option.value ~default:classifier.condition condition in
      let action = Option.value ~default:classifier.action action in
      let identifier = make_identifier ~condition ~action in
      {
        identifier;
        condition;
        action;
        prediction = Option.value ~default:classifier.prediction prediction;
        prediction_error = Option.value ~default:classifier.prediction_error prediction_error;
        fitness = Option.value ~default:classifier.fitness fitness;
        last_occurrence = Option.value ~default:classifier.last_occurrence last_occurrence;
        experience = Option.value ~default:classifier.experience experience;
        avg_action_set_size = Option.value ~default:classifier.avg_action_set_size avg_action_set_size;
        numerosity = Option.value ~default:classifier.numerosity numerosity;
        accuracy = Option.value ~default:classifier.accuracy accuracy;
      }

  let equal { identifier = id1; _ } { identifier = id2; _ } =
    Identifier.equal id1 id2

  let identifier { identifier; _ } =
    identifier

  let fitness { fitness; _ } =
    fitness

  let numerosity { numerosity; _ } =
    numerosity
end
