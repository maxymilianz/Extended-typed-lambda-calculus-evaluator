BUILD = ocamlbuild -I Tests

bad_typing_tests:
	$(BUILD) -pkg alcotest BadTypingTests.byte
	./BadTypingTests.byte

good_typing_tests:
	$(BUILD) -pkg alcotest GoodTypingTests.byte
	./GoodTypingTests.byte

tests: bad_typing_tests good_typing_tests

clean:
	ocamlbuild -clean
