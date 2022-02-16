open Evaluator
open Language
open TestUtilities
open Types


module GoodTypingTests = struct
    let test_arithmetic_exception () =
        let expression = Exception (
            "arithexc",
            NaturalType,
            (
                Try (
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
                )
            )
        ) in
        assert_equal_expressions ~actual:(evaluate expression)
                                 ~expected:(Natural 69)

    let test_natural_twice () =
        let expression = Application (
            Application (
                Abstraction (
                    ("function", AbstractionType (NaturalType, NaturalType)),
                    Abstraction (
                        ("number", NaturalType),
                        Application (
                            Variable "function",
                            Application (Variable "function", Variable "number")
                        )
                    )
                ),
                Abstraction (
                    ("number", NaturalType),
                    Add (
                        Variable "number",
                        Variable "number"
                    )
                )
            ),
            Natural 2
        ) in
        assert_equal_expressions ~actual:(evaluate expression)
                                 ~expected:(Natural 8)


    let test_id () =
        let abstraction = Abstraction (("parameter", BoolType), Variable "parameter") in
        assert_equal_expressions ~actual:(evaluate abstraction)
                                 ~expected:abstraction

    let test_is_zero () =
        let abstraction = Abstraction (("parameter", NaturalType),
                                       Equal (Natural 0, Variable "parameter")) in
        assert_equal_expressions ~actual:(evaluate abstraction)
                                 ~expected:abstraction


    let test_addition () =
        let expression = Add (Natural 19, Natural 98) in
        assert_equal_expressions ~actual:(evaluate expression)
                                 ~expected:(Natural 117)

    let test_multiplication () =
        let expression = Multiply (Natural 19, Natural 98) in
        assert_equal_expressions ~actual:(evaluate expression)
                                 ~expected:(Natural 1862)

    let test_subtraction () =
        let expression = Subtract (Natural 98, Natural 19) in
        assert_equal_expressions ~actual:(evaluate expression)
                                 ~expected:(Natural 79)

    let test_saturating_subtraction () =
        let expression = Subtract (Natural 19, Natural 98) in
        assert_equal_expressions ~actual:(evaluate expression)
                                 ~expected:(Natural 0)

    let test_division () =
        let expression = Divide (Natural 98, Natural 19) in
        assert_equal_expressions ~actual:(evaluate expression)
                                 ~expected:(Natural 5)

    (* equality tested in arithmetic exception test *)


    let test_true () =
        let expression = If (True, Natural 21, Natural 37) in
        assert_equal_expressions ~actual:(evaluate expression)
                                 ~expected:(Natural 21)

    let test_false () =
        let expression = If (False, Natural 21, Natural 37) in
        assert_equal_expressions ~actual:(evaluate expression)
                                 ~expected:(Natural 37)


    let test_recursion () =
        let factorial =
            Fix (
                Abstraction (
                    ("f", AbstractionType (NaturalType, NaturalType)),
                    Abstraction (
                        ("n", NaturalType),
                        If (
                            Equal (
                                Variable "n",
                                Natural 0
                            ),
                            Natural 1,
                            Multiply (
                                Variable "n",
                                Application (
                                    Variable "f",
                                    Subtract (
                                        Variable "n",
                                        Natural 1
                                    )
                                )
                            )
                        )
                    )
                )
            ) in
        let factorial_5 = Application (factorial, Natural 5) in
        assert_equal_expressions ~actual:(evaluate factorial_5)
                                 ~expected:(Natural 120)

    let test_is_even () =
        let is_even =
            Fix (
                Abstraction (
                    ("f", AbstractionType (NaturalType, BoolType)),
                    Abstraction (
                        ("n", NaturalType),
                        If (
                            Equal (
                                Variable "n",
                                Natural 0
                            ),
                            True,
                            If (
                                Equal (
                                    Variable "n",
                                    Natural 1
                                ),
                                False,
                                If (
                                    Application (
                                        Variable "f",
                                        Subtract (
                                            Variable "n",
                                            Natural 1
                                        )
                                    ),
                                    False,
                                    True
                                )
                            )
                        )
                    )
                )
            ) in
        let is_even_120 = Application (is_even, Natural 120) in
        assert_equal_expressions ~actual:(evaluate is_even_120)
                                 ~expected:True


    let test_unhandled_exception () =
        let expression = Exception (
            "exception",
            BoolType,
            If (True, Natural 98, Throw ("exception", False, NaturalType))
        ) in
        assert_equal_expressions ~actual:(evaluate expression)
                                 ~expected:(Natural 98)

    let test_exception_scope_exit () =
        let expression = Exception (
            "exception",
            BoolType,
            Try (
                Exception (
                    "exception",
                    BoolType,
                    Throw ("exception", True, BoolType)
                ),
                [("exception", "parameter", False)]
            )
        ) in
        assert_exception_scope_exit expression "exception"

    let test_multiple_handlers () =
        let expression = Exception (
            "e1",
            BoolType,
            Exception (
                "e2",
                NaturalType,
                Try (
                    Throw ("e1", True, BoolType),
                    [("e1", "p1", Variable "p1");
                     ("e2", "p2", False)]
                )
            )
        ) in
        assert_equal_expressions ~actual:(evaluate expression)
                                 ~expected:(True)
end


let () = run_tests {
    suite_name = "Good typing";
    chunks = [
        {
            chunk_name = "More complex tests";
            test_cases = [
                {
                    case_name = "Arithmetic exception";
                    test_function = GoodTypingTests.test_arithmetic_exception
                };
                {
                    case_name = "Addition applied twice";
                    test_function = GoodTypingTests.test_natural_twice
                };
            ]
        };
        {
            chunk_name = "Typed lambda calculus";
            test_cases = [
                {
                    case_name = "Id";
                    test_function = GoodTypingTests.test_id
                };
                {
                    case_name = "Is zero";
                    test_function = GoodTypingTests.test_is_zero
                };
            ]
        };
        {
            chunk_name = "Naturals";
            test_cases = [
                {
                    case_name = "Addition";
                    test_function = GoodTypingTests.test_addition
                };
                {
                    case_name = "Multiplication";
                    test_function = GoodTypingTests.test_multiplication
                };
                {
                    case_name = "Subtraction";
                    test_function = GoodTypingTests.test_subtraction
                };
                {
                    case_name = "Saturating subtraction";
                    test_function = GoodTypingTests.test_saturating_subtraction
                };
                {
                    case_name = "Division";
                    test_function = GoodTypingTests.test_division
                };
            ]
        };
        {
            chunk_name = "Booleans";
            test_cases = [
                {
                    case_name = "False";
                    test_function = GoodTypingTests.test_false
                };
                {
                    case_name = "True";
                    test_function = GoodTypingTests.test_true
                };
            ]
        };
        {
            chunk_name = "Fix";
            test_cases = [
                {
                    case_name = "Recursion";
                    test_function = GoodTypingTests.test_recursion
                };
                {
                    case_name = "Is even";
                    test_function = GoodTypingTests.test_is_even
                };
            ]
        };
        {
            chunk_name = "Exceptions";
            test_cases = [
                {
                    case_name = "Unhandled exception";
                    test_function = GoodTypingTests.test_unhandled_exception
                };
                {
                    case_name = "Exception scope exit";
                    test_function = GoodTypingTests.test_exception_scope_exit
                };
                {
                    case_name = "Multiple handlers";
                    test_function = GoodTypingTests.test_multiple_handlers
                };

                (* handling exception tested in arithmetic exception test *)
            ]
        }
    ]
}
