module Action = Action
(** The [Action] module defines a signature used to parameterise {!Learner}. *)

module Config = Config
(** The [Config] module defines the various configuration parameters for {!Learner}. *)

module Dict = Dict
(** Definitions pertaining to the core data structure used to hold the population of classifiers. *)

module Environment = Environment
(** The [Environment] module is used to construct values of the environment to feed the {!Learner}. *)

module Learner = Learner
(** The [Learner] module implements the XCS learning classifier system. *)

module Singlecore_dict = Singlecore_dict
(** {!Dict} implementation optimised for single core programs (or for OCaml < 5). *)

module Multicore_dict = Multicore_dict
(** {!Dict} implementation optimised for multi core programs (only for OCaml >= 5). *)

module Sensors = Sensors
(** The [Sensors] module defines a signature used to parameterise {!Learner}. *)
