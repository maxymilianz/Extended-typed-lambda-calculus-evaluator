To run the tests and see the program really works, one needs the Alcotest framework, which is installed by `opam install alcotest`.

I have tested it with OCaml `4.11.1`.

After installing Alcotest, run with `make tests`.

To track potentially thrown exceptions, use `TrackingTypechecker.ml` instead of `Typechecker.ml` (either rename `TrackingTypechecker` to `Typechecker` or change OCaml `open` instructions in every file; the interface of both files is the same).
