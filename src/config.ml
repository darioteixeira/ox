type classifier_initialization = {
  initial_prediction : float;
  initial_prediction_error : float;
  initial_fitness : float;
} [@@deriving yojson]

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
  wildcard_probability : float;
  mutation_probability : float;
  deletion_threshold : int;
  fitness_threshold : float;
  subsumption_threshold : int;
  exploration_probability : float;
  min_actions : int option;
  do_offspring_subsumption : bool;
  do_action_set_subsumption : bool;
  do_ga_only_when_exploring : bool;
  classifier_initialization : classifier_initialization;
} [@@deriving yojson]

let classifier_initialization_default = {
  initial_prediction = 0.;
  initial_prediction_error = 0.;
  initial_fitness = 0.;
}

let default = {
  max_population_size = 1000;
  discount_factor = 0.71;
  learning_rate = 0.15;
  accuracy_coefficient = 0.1;
  accuracy_power = 5.;
  prediction_error_threshold = 1.;
  age_threshold = 30.;
  crossover_probability = 0.75;
  offspring_fitness_multiplier = 0.1;
  wildcard_probability = 0.33;
  mutation_probability = 0.02;
  deletion_threshold = 20;
  fitness_threshold = 0.1;
  subsumption_threshold = 20;
  exploration_probability = 0.5;
  min_actions = None;
  do_offspring_subsumption = true;
  do_action_set_subsumption = true;
  do_ga_only_when_exploring = true;
  classifier_initialization = classifier_initialization_default;
}
