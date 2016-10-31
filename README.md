# Schemer

This repository contains the code that I wrote while reading the Schemer books:

- _The Little Schemer_, the original book
- _The Seasoned Schemer_, the sequel
- _The Reasoned Schemer_, on logic programming
- _The Little Prover_, on theorem proving

All four books use the Scheme language. I personally use [Gambit Scheme][1], but any Scheme should work. _The Reasoned Schemer_ relies on [miniKanren][1], a logic programming DSL. I never actually tried running the code in [reasoned.scm](reasoned.scm), but if I did I would probably try using Clojure's [core.logic][3] instead. _The Litter Prover_ looks at first like Common Lisp, but it is actually Scheme for a proof assistant called [J-Bob][4].

[1]: http://gambitscheme.org/wiki/index.php/Main_Page
[2]: http://minikanren.org
[3]: https://github.com/clojure/core.logic
[4]: https://github.com/the-little-prover/j-bob

## License

© 2016 Mitchell Kember

Schemer is available under the MIT License; see [LICENSE](LICENSE.md) for details.