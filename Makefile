BUILD = ocamlbuild -r -use-ocamlfind -use-menhir -pkg alcotest -pkg core -tag thread -I Parser -I Parser/tests -I Tests

clean_parser:
	rm -f Parser/Parser.ml
	rm -f Parser/Parser.mli

generate_parser: clean_parser
	corebuild -use-menhir Parser/Parser.ml
	mv _build/Parser/Parser.ml Parser/
	mv _build/Parser/Parser.mli Parser/

build: clean_parser
	$(BUILD) Main.byte

run: build
	./Main.byte example.in example.out

test_lexer: clean_parser
	$(BUILD) LexerTests.byte
	./LexerTests.byte

test_parser: clean_parser
	$(BUILD) ParserTests.byte
	./ParserTests.byte

bad_typing_tests:
	$(BUILD) -pkg alcotest BadTypingTests.byte
	./BadTypingTests.byte

good_typing_tests:
	$(BUILD) -pkg alcotest GoodTypingTests.byte
	./GoodTypingTests.byte

end_to_end_tests: build
	$(BUILD) -pkg alcotest EndToEndTests.byte
	./EndToEndTests.byte

tests: test_lexer test_parser bad_typing_tests good_typing_tests end_to_end_tests
	rm -f test.in
	rm -f test.out

clean: clean_parser
	ocamlbuild -clean
