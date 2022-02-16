open Language
open TestUtilities
open Typechecker
open Types


module BadTypingTests = struct
    let test_undefined_arithmetic_exception () =
        let expression = Try (
            Application (
                Application (
                    Abstraction (
                        ("x", NaturalType),
                        Abstraction (
                            ("y", NaturalType),
                            If (
                                Equal (
                                    Variable "y",
                                    Natural 0
                                ),
                                Throw (
                                    "arithexc",
                                    Natural 2137,
                                    NaturalType
                                ),
                                Divide (
                                    Variable "x",
                                    Variable "y"
                                )
                            )
                        )
                    ),
                    Natural 2
                ),
                Natural 0
            ),
            [
                "arithexc", "foo", Natural 69
            ]
        ) in
        assert_type_is_none (check_type expression)


    let test_undefined_variable () =
        assert_type_is_none (check_type (Variable "undefined"))

    let test_undefined_variable_in_body () =
        let abstraction = Abstraction (("parameter", BoolType), Variable "undefined") in
        assert_type_is_none (check_type abstraction)

    let test_bad_argument_type () =
        let application = Application (Abstraction (("parameter", BoolType), Variable "parameter"),
                                       Natural 2137) in
        assert_type_is_none (check_type application)

    let test_application_of_natural () =
        let application = Application (Natural 69, Natural 14) in
        assert_type_is_none (check_type application)


    let test_addition_of_bool_and_natural () =
        assert_type_is_none (check_type (Add (True, Natural 88)))

    let test_addition_of_bools () =
        assert_type_is_none (check_type (Add (True, False)))

    let test_addition_of_natural_and_bool () =
        assert_type_is_none (check_type (Add (Natural 88, False)))

    let test_multiplication_of_bools () =
        assert_type_is_none (check_type (Multiply (True, False)))

    let test_subtraction_of_bools () =
        assert_type_is_none (check_type (Subtract (True, False)))

    let test_division_of_bools () =
        assert_type_is_none (check_type (Divide (True, False)))

    let test_equality_of_bools () =
        assert_type_is_none (check_type (Equal (True, False)))


    let test_natural_condition () =
        assert_type_is_none (check_type (If (Natural 14, Natural 19, Natural 97)))

    let test_different_type_branches () =
        assert_type_is_none (check_type (If (True, Natural 88, False)))

    let test_undefined_variable_in_condition () =
        assert_type_is_none (check_type (If (Variable "undefined", True, False)))

    let test_undefined_variable_in_then () =
        assert_type_is_none (check_type (If (True, Variable "undefined", False)))


    let test_undefined_variable_in_fix () =
        assert_type_is_none (check_type (Fix (Variable "undefined")))

    let test_bool_in_fix () =
        assert_type_is_none (check_type (Fix (True)))


    let test_undefined_variable_after_exception () =
        let expression = Exception ("exception", BoolType, Variable "undefined") in
        assert_type_is_none (check_type expression)

    let test_throwing_undefined_exception () =
        let throw = Throw ("undefined", True, BoolType) in
        assert_type_is_none (check_type throw)

    let test_throwing_undefined_exception_with_undefined_variable () =
        let throw = Throw ("exception", Variable "variable", BoolType) in
        assert_type_is_none (check_type throw)

    let test_undefined_variable_in_try () =
        assert_type_is_none (check_type (Try (Variable "undefined", [])))

    let test_undefined_exception_handler () =
        let try_expression = Try (True, [("undefined", "parameter", False)]) in
        assert_type_is_none (check_type try_expression)

    let test_undefined_exception_handler_for_undefined_variable () =
        let try_expression = Try (Variable "variable", [("exception", "parameter", True)]) in
        assert_type_is_none (check_type try_expression)

    let test_bad_exception_argument_type () =
        let expression =
            Exception ("exception", BoolType, Throw ("exception", Natural 21, BoolType)) in
        assert_type_is_none (check_type expression)

    let test_undefined_variable_as_exception_argument () =
        let expression =
            Exception ("exception", BoolType, Throw ("exception", Variable "undefined", BoolType)) in
        assert_type_is_none (check_type expression)

    let test_undefined_variable_in_nonempty_try () =
        let expression = Exception (
            "exception",
            BoolType,
            Try (Variable "undefined", [("exception", "parameter", True)])
        ) in
        assert_type_is_none (check_type expression)

    let test_undefined_variable_in_handler () =
        let expression = Exception (
            "exception",
            BoolType,
            Try (True, [("exception", "parameter", Variable "undefined")])
        ) in
        assert_type_is_none (check_type expression)

    let test_different_type_handlers () =
        let expression = Exception (
            "1",
            BoolType,
            Exception (
                "2",
                BoolType,
                Try (
                    True,
                    [("1", "parameter", False); ("2", "parameter", Natural 37)]
                )
            )
        ) in
        assert_type_is_none (check_type expression)

    let test_bad_catched_exception_argument_type () =
        let expression = Exception (
            "exception",
            BoolType,
            Try (
                Throw ("exception", True, NaturalType),
                [("exception", "parameter", Add (Variable "parameter", Natural 14))]
            )
        ) in
        assert_type_is_none (check_type expression)

    let test_different_type_exception_and_handler () =
        let expression = Exception (
            "exception",
            BoolType,
            Try (
                Natural 88,
                [("exception", "parameter", True)]
            )
        ) in
        assert_type_is_none (check_type expression)

    let test_bad_exception_type () =
        let expression = Exception (
            "exception",
            BoolType,
            Add (
                Natural 666,
                Throw ("exception", True, BoolType)
            )
        ) in
        assert_type_is_none (check_type expression)
end


let () = run_tests {
    suite_name = "Bad typing";
    chunks = [
        {
            chunk_name = "More complex test";
            test_cases = [
                {
                    case_name = "Undefined arithmetic exception";
                    test_function = BadTypingTests.test_undefined_arithmetic_exception
                };
            ]
        };
        {
            chunk_name = "Typed lambda calculus";
            test_cases = [
                {
                    case_name = "Undefined variable";
                    test_function = BadTypingTests.test_undefined_variable
                };
                {
                    case_name = "Undefined variable in body";
                    test_function = BadTypingTests.test_undefined_variable_in_body
                };
                {
                    case_name = "Bad argument type";
                    test_function = BadTypingTests.test_bad_argument_type
                };
                {
                    case_name = "Application of natural";
                    test_function = BadTypingTests.test_application_of_natural
                };
            ]
        };
        {
            chunk_name = "Naturals";
            test_cases = [
                {
                    case_name = "Bool + natural";
                    test_function = BadTypingTests.test_addition_of_bool_and_natural
                };
                {
                    case_name = "Bool + bool";
                    test_function = BadTypingTests.test_addition_of_bools
                };
                {
                    case_name = "Natural + bool";
                    test_function = BadTypingTests.test_addition_of_natural_and_bool
                };
                {
                    case_name = "Bool * bool";
                    test_function = BadTypingTests.test_multiplication_of_bools
                };
                {
                    case_name = "Bool - bool";
                    test_function = BadTypingTests.test_subtraction_of_bools
                };
                {
                    case_name = "Bool / bool";
                    test_function = BadTypingTests.test_division_of_bools
                };
                {
                    case_name = "Bool = bool";
                    test_function = BadTypingTests.test_equality_of_bools
                };
            ]
        };
        {
            chunk_name = "Booleans";
            test_cases = [
                {
                    case_name = "If natural then natural else natural";
                    test_function = BadTypingTests.test_natural_condition
                };
                {
                    case_name = "If bool then natural else bool";
                    test_function = BadTypingTests.test_different_type_branches
                };
                {
                    case_name = "If undefined then bool else bool";
                    test_function = BadTypingTests.test_undefined_variable_in_condition
                };
                {
                    case_name = "If bool then undefined else bool";
                    test_function = BadTypingTests.test_undefined_variable_in_then
                };
            ]
        };
        {
            chunk_name = "Fix";
            test_cases = [
                {
                    case_name = "Fix undefined";
                    test_function = BadTypingTests.test_undefined_variable_in_fix
                };
                {
                    case_name = "Fix bool";
                    test_function = BadTypingTests.test_bool_in_fix
                };
            ]
        };
        {
            chunk_name = "Exceptions";
            test_cases = [
                {
                    case_name = "exception in undefined variable";
                    test_function = BadTypingTests.test_undefined_variable_after_exception
                };
                {
                    case_name = "throw undefined";
                    test_function = BadTypingTests.test_throwing_undefined_exception
                };
                {
                    case_name = "throw undefined (undefined)";
                    test_function =
                        BadTypingTests.test_throwing_undefined_exception_with_undefined_variable
                };
                {
                    case_name = "undefined variable in try";
                    test_function = BadTypingTests.test_undefined_variable_in_try
                };
                {
                    case_name = "undefined exception handler";
                    test_function = BadTypingTests.test_undefined_exception_handler
                };
                {
                    case_name = "undefined exception handler for undefined variable";
                    test_function =
                        BadTypingTests.test_undefined_exception_handler_for_undefined_variable
                };
                {
                    case_name = "Bad exception argument type";
                    test_function = BadTypingTests.test_bad_exception_argument_type
                };
                {
                    case_name = "Undefined variable as exception argument";
                    test_function = BadTypingTests.test_undefined_variable_as_exception_argument
                };
                {
                    case_name = "Undefined variable in nonempty try";
                    test_function = BadTypingTests.test_undefined_variable_in_nonempty_try
                };
                {
                    case_name = "Undefined variable in handler";
                    test_function = BadTypingTests.test_undefined_variable_in_handler
                };
                {
                    case_name = "different type handlers";
                    test_function = BadTypingTests.test_different_type_handlers
                };
                {
                    case_name = "Bad catched exception argument type";
                    test_function = BadTypingTests.test_bad_catched_exception_argument_type
                };
                {
                    case_name = "Different type exception and handler";
                    test_function = BadTypingTests.test_different_type_exception_and_handler
                };
                {
                    case_name = "Bad exception type";
                    test_function = BadTypingTests.test_bad_exception_type
                };
            ]
        };
    ]
}
