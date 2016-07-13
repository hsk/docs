all:
	ocamlyacc parser.mly
	ocamllex lexer.mll
	rm -rf parser.mli
	ocamlc -o calc unix.cma ast.ml parser.ml lexer.ml utils.ml main.ml
	./calc
clean:
	rm -rf calc *.cm* calc lexer.ml parser.ml out*
