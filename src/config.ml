type classifier_initialization = {
  initial_prediction : float;
  initial_prediction_error : float;
  initial_fitness : float;
  wildcard_probability : float;
} [@@deriving repr]

type t = {
  max_population_size : int;
  discount_factor : float;
  learning_rate : float;
  accuracy_coefficient : float;
  accuracy_power : float;
  prediction_error_threshold : float;
  age_threshold : float;
  crossover_probability : float;
  offspring_fitness_multiplier : float;
  mutation_probability : float;
  deletion_threshold : int;
  fitness_threshold : float;
  subsumption_threshold : int;
  exploration_probability : float;
  min_actions : int;
  do_offspring_subsumption : bool;
  do_action_set_subsumption : bool;
  classifier_initialization : classifier_initialization;
} [@@deriving repr]
