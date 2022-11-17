Ox
==

![Ox logo](logo/ox_logo.png?raw=true "Ox logo")

OCaml implementation of the XCS learning classifier system[^1].

A learning classifier system is a rule-based machine learning technique that
can be applied to most categories of machine learning problems, including
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
computationally demanding component of a learning classifier system, but
fortunately it also happens to be an embarrassingly parallel problem that is
well suited to the shared-memory parallelism featured by OCaml 5.


Building
--------

```
opam update
opam switch create . 4.13.1 --no-install
eval $(opam env)
opam install . --deps-only --with-test --locked
```

To update the OPAM lock file with new or more recent packages, follow this recipe:

```
opam update
opam upgrade
opam install . --deps-only --with-test
opam lock .
```

[^1]: [*Classifier Fitness Based on Accuracy*, by Stewart W. Wilson](https://doi.org/10.1162/evco.1995.3.2.149)
[^2]: [*An Algorithmic Description of XCS*, by Martin V. Butz and Stewart W. Wilson](https://dx.doi.org/10.1007/s005000100111)
