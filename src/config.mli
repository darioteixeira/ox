(** This module declares the various parameters used to configure the {!Learner}. *)

type classifier_initialization = {
  initial_prediction : float;
  (** Parameter [p_I]: Initial value for the prediction parameter [p] in new classifiers.
      (Usually 0) *)
  initial_prediction_error : float;
  (** Parameter [ε_I]: Initial value for the error estimate parameter [ε] in new classifiers.
      (Usually 0) *)
  initial_fitness : float;
  (** Parameter [F_I]: Initial value for the fitness parameter [F] in new classifiers.
      (Usually 0) *)
} [@@deriving yojson]

type t = {
  max_population_size : int;
  (** Parameter [N]: Maximum population sized in terms of micro-classifiers,
      ie, the sum of the numerosity of macro-classifiers. *)
  discount_factor : float;
  (** Parameter [γ]: In multi-step problems, this is the discount
      factor used for updating classifier predictions.
      (Usually 0.71) *)
  learning_rate : float;
  (** Parameter [β]: Learning rate for classifier parameters [p], [ε], [f], and [as].
      (Normally between 0.1 and 0.2). *)
  accuracy_coefficient : float;
  (** Parameter [α]: Used for calculating the fitness of a classifier.
      (Normally 0.1) *)
  accuracy_power : float;
  (** Parameter [ν]: Used for calculating the fitness of a classifier.
      (Typically 5) *)
  prediction_error_threshold : float;
  (** Parameter [ε_0]: Used for calculating the fitness of a classifier.
      (Typically about 1% of the maximum value of the reward) *)
  age_threshold : float;
  (** Parameter [θ_GA]: This is the Genetic Algorithm age threshold.
      The GA is applied on a set when the average time since the
      last GA invocation for the set is greater than [θ_GA].
      (Usually between 25 and 50) *)
  crossover_probability : float;
  (** Parameter [χ]: The probability of applying crossover in the GA.
      (Usually between 0.5 and 1) *)
  offspring_fitness_multiplier : float;
  (** For classifiers created during a run of the genetic algorithm,
      their fitness will be multiplied by this value.
      (In the reference paper, this is actually a constant with a value of 0.1) *)
  wildcard_probability : float;
  (** Parameter [P_#]: This is the probability of using a wildcard (also known as a "don't care"
      and usually represented by the [#] symbol) in one attribute in the condition string when covering.
      (Usually around 0.33) *)
  mutation_probability : float;
  (** Parameter [μ]: The probability of mutating an allele
      in the offspring when running the GA.
      (Usually between 0.01 and 0.05) *)
  deletion_threshold : int;
  (** Parameter [θ_del]: This is the deletion threshold. If the experience
      of a classifier is greater than [θ_del], its fitness may be considered
      in its probability of deletion.
      (Usually around 20) *)
  fitness_threshold : float;
  (** Parameter [δ]: This is the fraction of the mean fitness in whole population below
      which the fitness of a classifier may be considered in its probability of deletion.
      (Usually 0.1) *)
  subsumption_threshold : int;
  (** Parameter [θ_sub]: This is the subsumption threshold. The experience of a classifier
      must be greater than [θ_sub] for it to be able to subsume another classifier.
      (Usually around 20 or higher) *)
  exploration_probability : float;
  (** Parameter [p_explr]: During the selection of an action, this is the probability
      of going for exploration by choosing an action with random uniform probability.
      (Usually around 0.5) *)
  min_actions : int option;
  (** Parameter [θ_mna]: This is the minimum number of actions that must be present
      in the match set [M]. If the number of actions is smaller than this minimum,
      then covering will occur. If set to [None], then the minimum is equal to the
      number of possible actions (this is also the recommended default).
      You may however set it to a smaller number using the [Some] variant. *)
  do_offspring_subsumption : bool;
  (** Parameter [doGASubsumption]: Boolean that specifies whether offspring
      are to be tested for possible logical subsumption by their parents. *)
  do_action_set_subsumption : bool;
  (** Parameter [doActionSetSubsumption]: Boolean that specifies whether
      action sets are to be tested for subsuming classifiers. *)
  classifier_initialization : classifier_initialization;
  (** Initial parameters for new classifiers. *)
} [@@deriving yojson]

val default_classifier_initialization : classifier_initialization

val default : t
