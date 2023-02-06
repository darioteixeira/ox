module Action = Action
(** The [Action] module defines a signature used to parameterise {!Learner}. *)

module Config = Config
(** The [Config] module defines the various configuration parameters for {!Learner}. *)

module Environment = Environment
(** The [Environment] module is used to construct values of the environment to feed the {!Learner}. *)

module Learner = Learner
(** The [Learner] module implements the XCS learning classifier system. *)

module Multicore_config = Multicore_config
(** Defines configuration options for multi-core Learners. *)

module Sensors = Sensors
(** The [Sensors] module defines a signature used to parameterise {!Learner}. *)
