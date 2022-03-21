Ox
==

OCaml implementation of the XCS learning classifier system[^1][^2].


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
