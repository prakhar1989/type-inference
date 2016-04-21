all:
	ocamlbuild -j 0 -r -use-menhir repl.native
	@mv repl.native repl

infer:
	ocamlbuild -j 0 -r -use-ocamlfind infer.native

.PHONY: clean
clean:
	ocamlbuild -clean
