open Evaluator
open FileUtilities
open Language
open Types

open Alcotest


type test_case = {
    case_name : string;
    test_function : unit -> unit;
}


type test_chunk = {
    chunk_name : string;
    test_cases : test_case list;
}


type test_suite = {
    suite_name : string;
    chunks : test_chunk list;
}


let process_case case = test_case case.case_name `Quick case.test_function


let process_chunk chunk = chunk.chunk_name, (List.map process_case chunk.test_cases)


let run_tests suite = run suite.suite_name (List.map process_chunk suite.chunks)


let assert_type_is_none = function
    | None -> ()
    | Some type_value ->
        failwith ("Expected None, got value:\n" ^
                  (type_to_string type_value) ^ "\n")


let assert_equal_expressions ~actual ~expected =
    if actual = expected then
        ()
    else
        failwith ("Got value:\n" ^
                  (expression_to_string actual) ^ "\n" ^
                  "Expected value:\n" ^
                  (expression_to_string expected) ^ "\n")


let assert_exception_scope_exit expression exception_name =
    try
        evaluate expression; ()
    with
        ExceptionScopeExit argument as actual_exception ->
            if argument = exception_name then
                ()
            else
                raise actual_exception


let assert_main_output expression_string expected_output =
    let input_filename = "test.in"
    and output_filename = "test.out" in
    write_to_file expression_string input_filename;
    Sys.command ("./Main.byte " ^ input_filename ^ " " ^ output_filename);
    let output = file_to_string output_filename in
    if output = expected_output then
        ()
    else
        failwith ("Main got input:\n" ^
                  expression_string ^ "\n" ^
                  "and produced output:\n" ^
                  output ^ "\n" ^
                  "instead of:\n" ^
                  expected_output ^ "\n")


let assert_main_fails expression_string =
    assert_main_output expression_string "None, None"
