all:
	coqc hello.v
	rm -rf hello.mli
	ocamlc -o hello hello.ml main.ml
	./hello

clean:
	rm -rf *.glob *.vo hello.mli hello.ml *.cm* hello .*.aux




