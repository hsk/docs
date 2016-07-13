all:
	ocamlyacc parser.mly
	ocamllex lexer.mll
	rm -rf parser.mli
	ocamlfind ocamlc -o calc ast.ml parser.ml lexer.ml utils.ml main.ml -package llvm.executionengine -linkpkg
	./calc
clean:
	rm -rf calc *.cm* calc lexer.ml parser.ml
