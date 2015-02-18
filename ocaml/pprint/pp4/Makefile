all:
	ocamlyacc parser.mly
	ocamllex lexer.mll
	rm parser.mli
	ocamlc str.cma e.ml parser.ml lexer.ml main.ml
	./a.out test/t

clean:
	rm -rf *.cm* parser.ml lexer.ml a.out
