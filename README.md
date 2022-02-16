Structure:
I find it truly self-explanatory.
`Parser` folder contains lower level `Lexer.mll` & `Parser.mly` files and higher level `ParserProxy` and tests.
`example.in` and `example.out` are the default files for input (expression in the format explained below) and output (type option, value option), which the program is launched with.

Compilation:
Prerequisites: ocamlbuild, ocamlfind, core, alcotest and menhir packages installed (each one may be installed with `opam`). I tested the program with OCaml `4.13.1`.
`make build` only compiles `Main.byte` without tests.

Running:
To run `./Main.byte example.in example.out` - `make run`. Of course `Main` may be run manually with different arguments.
To run all tests - `make tests`.
In `Makefile` more commands for running specific test suites can be found - e.g. `make test_lexer` for lexer tests only.

Input expression format:
reverse polish notation, where the first argument has to be in parentheses (e.g. `+ (1) 2` instead of `1 + 2`).
Apart from this, the expressions are written in an expected, straightforward way.
I think it's best to go through `Parser.ml` to effectively see the CFG of the language. It may also be helpful to see e.g. `test_recursion` test in `Tests/EndToEndTests.ml`.
All typenames (`bool`, `nat`), keywords (`function`, `if`, `then`, `else`, `fix`, `exception`, `of`, `in`, `raise`, `as`, `try`, `catch`) and predefined values (`false`, `true`) can only be written in lower case.
In application, the first term (the function) has to be in parentheses, even if it's e.g. a variable (`(f) 1`, not `f 1`).
A composite type (e.g. `nat -> nat`) has to be in parentheses, e.g. `function f : (nat -> nat) -> f` is an ID function on arguments of type `nat -> nat`. (The same `->` is used both for type constructor `->` and as a delimiter between the function parameter and its body.)
Body of a catch clause has to be in parentheses (`try ... catch exc arg -> (...)`).

The exercise is realized in 100%. Extraordinarily extensive testing confirms its correctness.