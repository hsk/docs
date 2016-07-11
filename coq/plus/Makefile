all:
	coqc plus.v
	rm -rf plus.mli
	ocamlc -o plus plus.ml main.ml
	./plus

clean:
	rm -rf *.glob *.vo .*.aux plus.mli plus.ml *.cm* plus




