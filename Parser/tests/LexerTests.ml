open ParserProxy
open TestUtilities


module ReadTest = struct
    let lex_string value =
        let lexbuf = Lexing.from_string value in
        Lexer.read lexbuf

    let assert_equal_tokens ~actual ~expected =
        if actual = expected then
            ()
        else
            failwith "Not equal parser tokens."

    let test_function_keyword () =
        assert_equal_tokens ~actual:(lex_string "function\n")
                            ~expected:(Parser.FUNCTION)

    let test_natural () =
        assert_equal_tokens ~actual:(lex_string "2137\n")
                            ~expected:(Parser.NATURAL 2137)

    let test_plus () =
        assert_equal_tokens ~actual:(lex_string "+\n")
                            ~expected:(Parser.PLUS)

    let test_id () =
        assert_equal_tokens ~actual:(lex_string "x\n")
                            ~expected:(Parser.ID "x")
end


let () = run_tests {
    suite_name = "Lexer";
    chunks = [
        {
            chunk_name = "lexing";
            test_cases = [
                {
                    case_name = "function keyword";
                    test_function = ReadTest.test_function_keyword
                };
                {
                    case_name = "natural";
                    test_function = ReadTest.test_natural
                };
                {
                    case_name = "+";
                    test_function = ReadTest.test_plus
                };
                {
                    case_name = "id";
                    test_function = ReadTest.test_id
                };
            ]
        }
    ]
}
