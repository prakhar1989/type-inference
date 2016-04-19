infer:
	ocamlbuild -j 0 -r -use-ocamlfind infer.native

.PHONY: clean
clean:
	ocamlbuild -clean
