all:
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex lexer.mll
	ocamlc str.cma ast.ml parser.ml lexer.ml scc.ml main.ml -o yaccdep

clean:
	rm -rf yaccdep *.cm* parser.ml lexer.ml

