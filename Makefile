all:
	ocamlbuild -j 0 -r -use-menhir repl.native

infer:
	ocamlbuild -j 0 -r -use-ocamlfind infer.native

.PHONY: clean
clean:
	ocamlbuild -clean
