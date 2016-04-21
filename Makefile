all:
	ocamlbuild -j 0 -r -use-menhir repl.native
	@mv repl.native repl

.PHONY: test
test:
	ocamlbuild -j 0 -r -pkg alcotest -use-ocamlfind test.native
	@mv ./test.native test
	@./test


.PHONY: clean
clean:
	ocamlbuild -clean
