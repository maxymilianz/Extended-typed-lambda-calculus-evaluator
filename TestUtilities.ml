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


let assert_equal_types_and_values ~actual ~expected =
    if actual = expected then
        ()
    else
        let actual_maybe_type, actual_maybe_value = actual
        and expected_maybe_type, expected_maybe_value = expected in
        if actual_maybe_type = expected_maybe_type then
            failwith ("Got value:\n" ^
                     (maybe_expression_to_string actual_maybe_value) ^ "\n" ^
                     "Expected value:\n" ^
                     (maybe_expression_to_string expected_maybe_value) ^ "\n")
        else  (* actual_maybe_type <> expected_maybe_type *)
            failwith ("Got type:\n" ^
                     (maybe_type_to_string actual_maybe_type) ^ "\n" ^
                     "Expected type:\n" ^
                     (maybe_type_to_string expected_maybe_type) ^ "\n")
