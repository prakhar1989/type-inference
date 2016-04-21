all:
	ocamlbuild -j 0 -r -use-menhir repl.native
	@mv repl.native repl

test:
	ocamlbuild -j 0 -r -use-ocamlfind test.native
	@mv test.native test

.PHONY: clean
clean:
	ocamlbuild -clean
