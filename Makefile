infer:
	ocamlc -o infer.out type_infer.ml

.PHONY: clean
clean:
	rm *.cm*
