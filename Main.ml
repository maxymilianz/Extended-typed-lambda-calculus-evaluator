open Evaluator
open FileUtilities
open Typechecker


let check_type_and_evaluate expression =
    match check_type expression with
    | None -> (None, None)
    | some_type_value ->
        (some_type_value, Some (evaluate expression))


let () =
    let expression_string = file_to_string Sys.argv.(1) in
    let expression = ParserProxy.parse_string expression_string in
    let maybe_type, maybe_expression = check_type_and_evaluate expression in
    let output_string = (Types.maybe_type_to_string maybe_type) ^ ", " ^ (Language.maybe_expression_to_string maybe_expression) in
    write_to_file output_string Sys.argv.(2)
