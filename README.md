Ox
==

![Ox logo](logo/ox_logo.png?raw=true "Ox logo")

OCaml implementation of the XCS learning classifier system[^1].

A learning classifier system is a rule-based machine learning technique that
can be applied to most categories of machine learning problems, namely
supervised learning, unsupervised learning, and reinforcement learning.
Compared to state-of-the-art techniques based on deep learning, the big
advantage of learning classifier systems is that the rules they learn are
understandable and even tweakable by humans.

The XCS system is an importante milestone in the development of learning
classifier systems, and this project is a pure-OCaml implementation that
closely follows (with some minor deviations) the algorithmic description
of XCS given in [^2].

Of note is that the project can (optionally) take advantage of the multi-core
support offered by OCaml 5. By a long margin, rule matching is the most
computationally demanding component of a learning classifier system.
Fortunately it also happens to be an embarrassingly parallel problem that is
well suited to the shared-memory parallelism introduced by OCaml 5.

Features
--------

 * Multicore support with OCaml 5.
 * Configurable types for the *sensors* (not just the usual boolean).
 * All XCS hyper-parameters are configurable.

Building
--------

Ox requires (at least) OCaml 5. After cloning this repository or downloading
the package, change the working directory to the root of the project and run
the following commands to build Ox under a local switch:

```
opam update
opam switch create . 5.0.0 --no-install
eval $(opam env)
opam install . --deps-only --with-test
make build
```

Examples
--------

The directory `examples` contains some very simple but complete examples to get
you started experimenting with the library:

 * `multiplexer`: A supervised learning example, where Ox must learn to pick
   the correct line for a multiplexer whose input consists of an n-bit selector plus
   the 2^n input lines.

 * `tic-tac-toe`: A reinforcement learning example, where Ox (hopefully) learns
   an optimal strategy for playing Tic-tac-toe by repeatedly playing against itself.

Usage
-----

This section is purposefully brief; you are strongly encouraged to read the
examples for a more through understanding of how the library is meant to be
used.

Anyway, typical usage starts with the creation of the Ox learner module by
invoking either the `Ox.Learner.Make_singlecore` functor (for the single-core
version of the learner), or the `Ox.Learner.Make_multicore` functor (for the
multi-core version of the learner). The former takes two parameters:
the definition of the *sensors* (ie, the input) and the definition of the
*actions* (ie, the output). The latter takes three parameters: the first two
are the same as in the single-core version, and the third parameter defines
some configuration options relevant only to the multi-core version.

Once you've created the learner module -- let's call it `Learner` -- you
must invoke the `Learner.create` function to create a new instance of a
learner. What typically comes next depends on whether you are using the
library for classification (supervised learning) or for a reinforcement
learning problem. The former are single-step problems, whereas the latter
are usually (though not necessarily) multi-step problems.

For single step problems, the typical usage pattern is as follows:

```
while not termination condition:
  Learner.provide_environment
  Learner.provide_final_feedback
```

Whereas for multi-step problems, the usage pattern includes providing
intermediate feedback to the learner before the final step is reached:

```
while not termination condition:
  repeat until final step:
    Learner.provide_environment
    Learner.provide_intermediate_feedback
  Learner.provide_final_feedback
```

Note that API offered by the `Learner` module is imperative; functions
such as `Learner.provide_environment` and `Learner.provide_final_feedback`
modify the internal state of the learner.

API documentation
-----------------

The API is documented and available online [here](https://darioteixeira.github.io/ox/apidoc/index.html).
You may also build it locally by running `make doc`.

Differences between Ox and canonical XCS
----------------------------------------

Ox is a mostly faithful implementation of the algorithmic description of XCS
given in [^2]. The biggest deviation concerns the population size: Ox allows
for the population to increase beyond the configured maximum size in
intermediate computations; the population is only culled to the prescribed
limit when feedback is given either with
`Learner.provide_intermediate_feedback` or `Learner.provide_final_feedback`.
Performance is the primary motivation behind this deviation: culling is an
expensive operation, and since Ox uses an hash table to store the population of
classifiers, we can treat the maximum population size as a soft limit which is
perfectly fine to step over during intermediate computations. In contrast, the
population size is a hard limit for implementations of XCS based on a
fixed-length array, which explains their need for much more frequent expensive
cullings.

[^1]: [*Classifier Fitness Based on Accuracy*, by Stewart W. Wilson](https://doi.org/10.1162/evco.1995.3.2.149)
[^2]: [*An Algorithmic Description of XCS*, by Martin V. Butz and Stewart W. Wilson](https://dx.doi.org/10.1007/s005000100111)
