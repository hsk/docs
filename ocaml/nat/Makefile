all:
	ocamlyacc parser.mly
	ocamllex lexer.mll
	rm parser.mli
	ocamlc -g ast.ml parser.ml lexer.ml main.ml -o nat

clean:
	rm -f *.cm* parser.ml lexer.ml nat *.o