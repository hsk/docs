all:
	ocamlfind ocamlc -package ppx_deriving.show ../jData.ml -dsource 2> jData.ml
	-ocamlfind ocamlc -package ppx_deriving.show ../jCode.ml -I .. -c -dsource 2> jCode.ml
	ocamlc str.cma setup.ml
	./a.out
	rm *.cm* ./a.out

