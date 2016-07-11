all:
	coqc fact.v
	rm -rf fact.mli
	ocamlc -o fact fact.ml main.ml
	./fact

clean:
	rm -rf *.glob *.vo fact.mli fact.ml *.cm* .*.aux fact




