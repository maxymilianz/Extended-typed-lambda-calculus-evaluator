open Evaluator
open Typechecker


let check_type_and_evaluate expression =
    match check_type expression with
    | None -> (None, None)
    | some_type_value ->
        (some_type_value, Some (evaluate expression))
