open TestUtilities


module EndToEndTests = struct
    let test_undefined_arithmetic_exception () =
        assert_main_fails "
            try
                ((function x : nat ->
                    function y : nat ->
                        / (x) y) 0) 2
            catch e argument -> (2137)"

    let test_undefined_variable () =
        assert_main_fails "x"

    let test_undefined_variable_in_body () =
        assert_main_fails "function x : nat -> y"


    let test_arithmetic_exception () =
        assert_main_output "
            exception f of bool in
            exception e of nat in
            try
                ((function x : nat ->
                    function y : nat ->
                        / (x) y) 0) 2
            catch e argument -> (2137)
                | f arg1 -> (14)" "Natural, 0"

    let test_recursion () =
        assert_main_output "
            (fix function f : (nat -> nat) ->
                function n : nat ->
                    if = (n) 0 then 1
                    else * (n) ((f) (- (n) 1))) 5" "Natural, 120"
end


let () = run_tests {
    suite_name = "End to end tests";
    chunks = [
        {
            chunk_name = "Typechecking should fail";
            test_cases = [
                {
                    case_name = "Undefined arithmetic exception";
                    test_function = EndToEndTests.test_undefined_arithmetic_exception
                };
                {
                    case_name = "Undefined variable";
                    test_function = EndToEndTests.test_undefined_variable
                };
                {
                    case_name = "Undefined variable in body";
                    test_function = EndToEndTests.test_undefined_variable_in_body
                }
            ]
        };
        {
            chunk_name = "Should work";
            test_cases = [
                {
                    case_name = "Arithmetic exception";
                    test_function = EndToEndTests.test_arithmetic_exception
                };
                {
                    case_name = "Recursion";
                    test_function = EndToEndTests.test_recursion
                }
            ]
        }
    ]
}