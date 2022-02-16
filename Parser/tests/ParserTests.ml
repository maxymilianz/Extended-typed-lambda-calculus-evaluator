open Language
open ParserProxy
open TestUtilities
open Types


module ParseTest = struct
    let test_variable () =
        assert_equal_expressions
        ~actual:(parse_string "x")
        ~expected:(Variable "x")

    let test_abstraction () =
        assert_equal_expressions
        ~actual:(parse_string "function x : bool -> x")
        ~expected:(Abstraction (("x", BoolType), Variable "x"))

    let test_abstraction_application () =
        assert_equal_expressions
        ~actual:(parse_string "(function x : bool -> x) y")
        ~expected:(Application (Abstraction (("x", BoolType), Variable "x"), Variable "y"))


    let test_natural () =
        assert_equal_expressions
        ~actual:(parse_string "2137")
        ~expected:(Natural 2137)

    let test_addition () =
        assert_equal_expressions
        ~actual:(parse_string "+ (21) 37")
        ~expected:(Add (Natural 21, Natural 37))

    let test_complex_arithmetics () =
        assert_equal_expressions
        ~actual:(parse_string "+ (* (21) 14) - (37) 88")
        ~expected:(Add (Multiply (Natural 21, Natural 14), Subtract (Natural 37, Natural 88)))

    let test_operator_connectivity () =
        assert_equal_expressions
        ~actual:(parse_string "* (- (19) 98) 97")
        ~expected:(Multiply (Subtract (Natural 19, Natural 98), Natural 97))


    let test_complex_arithmetics_and_booleans () =
        assert_equal_expressions
        ~actual:(parse_string "if = (21) 14 then + (37) 19 else * (88) 98")
        ~expected:(If (Equal (Natural 21, Natural 14), Add (Natural 37, Natural 19), Multiply (Natural 88, Natural 98)))

    let test_nested_if () =
        assert_equal_expressions
        ~actual:(parse_string "if true then if false then 0 else 1 else 2")
        ~expected:(If (True, If (False, Natural 0, Natural 1), Natural 2))
end


let () = run_tests {
    suite_name = "Parser";
    chunks = [
        {
            chunk_name = "parsing";
            test_cases = [
                { case_name = "variable";
                  test_function = ParseTest.test_variable; };
                { case_name = "abstraction";
                  test_function = ParseTest.test_abstraction; };
                { case_name = "abstraction application";
                  test_function = ParseTest.test_abstraction_application; };

                { case_name = "natural";
                  test_function = ParseTest.test_natural; };
                { case_name = "addition";
                  test_function = ParseTest.test_addition; };
                { case_name = "complex arithmetics";
                  test_function = ParseTest.test_complex_arithmetics; };
                { case_name = "operator connectivity";
                  test_function = ParseTest.test_operator_connectivity; };

                { case_name = "complex arithmetics and booleans";
                  test_function = ParseTest.test_complex_arithmetics_and_booleans; };
                { case_name = "nested if";
                  test_function = ParseTest.test_nested_if; };
            ]
        }
    ]
}
