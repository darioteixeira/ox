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

Of note is that the project can (optionally) take advantage of the multicore
support offered by OCaml 5. By a long margin, rule matching is the most
computationally demanding component of a learning classifier system.
Fortunately it also happens to be an embarrassingly parallel problem that is
well suited to the shared-memory parallelism introduced by OCaml 5.

Features
--------

 * (Optional) multicore support with OCaml 5.
 * Configurable types for the *sensors* (not just the usual boolean).
 * All XCS hyper-parameters are configurable.

Building
--------

Assuming you are running OPAM >= 2.1, you can run the commands below to build
Ox using the first beta version of OCaml 5 under a local switch (beware that
these instructions will **not** work under older versions of OPAM). Note that
strictly speaking, the main package of Ox (under the `src` directory) can also
be built with OCaml < 5. It's only the multicore-enabling subpackage (under
the `src_multicore` directory) that requires OCaml 5. Nevertheless, in this
README we will assume you are running OCaml 5.

```
opam update
opam switch create . 5.0.0~beta1 --no-install
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
invoking the `Ox.Learner.Make` functor. This functor takes three parameters:
the definition of the *sensors* (ie, the input), the definition of the
*actions* (ie, the output), and finally, the definition of the data structure
used to hold the population of classifiers. Note that the final parameter
may seem a bit superfluous at first -- why should it be your responsibility
as a user of the library to worry about this? -- but it exists for the sole
purpose of supporting both single core and multicore applications without
forcing the core Ox library to depend on OCaml 5 (please see the section below
for more details).

Once you've created the learner module -- let's call it `Learner` -- you
must invoke the `Learner.create` function to create a new instance of a
learner. What typically comes next dependes on whether you are using the
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

Single core vs multicore
------------------------

In the current version of Ox, multicore support is entirely optional, and the
core Ox library is actually compatible with OCaml 4.x. This was achieved by
adding a third parameter to the functor `Ox.Learner.Make` that instantiates a
learner.

If you are running OCaml 4.x or don't require multicore support, you can just
provide `Ox.Singlecore_dict.Make` as the third argument to the functor (as you
may have guessed from its name, the third argument is itself a functor, though
you probably don't need to worry about this detail).

If you want to leverage the multicore support, then you need to create that
third argument using the `Ox_multicore.Multicore_dict.Make` functor. You'll
need to provide the number of domains and the task pool to be used by Ox.
In the example below, we actually create a new task pool, though you
may of course also reuse an existing one:

```ocaml
module Dict = Ox_multicore.Multicore_dict.Make (struct
  let num_domains = 4
  let task_pool = Domainslib.Task.setup_pool ~num_domains:(num_domains - 1) ()
end)
```

(Note that for Ox, `num_domains` declares the **total** number of domains,
including the current one. In contrast, in the current version of
`Domainslib.Task.setup_pool`, the `num_domains` argument is to be interpreted
as the number of **additional** domains. Yes, this is confusing, but the APIs
are still in flux.)

[^1]: [*Classifier Fitness Based on Accuracy*, by Stewart W. Wilson](https://doi.org/10.1162/evco.1995.3.2.149)
[^2]: [*An Algorithmic Description of XCS*, by Martin V. Butz and Stewart W. Wilson](https://dx.doi.org/10.1007/s005000100111)
