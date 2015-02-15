all:
	ocamlyacc parser.mly
	ocamllex lexer.mll
	ocamlc str.cma terminfo.mli terminfo.ml warnings.mli warnings.ml location.mli location.ml asttypes.mli misc.mli misc.ml longident.mli longident.ml parsetree.mli ast_helper.mli ast_helper.ml config.mli config.ml clflags.mli clflags.ml syntaxerr.mli syntaxerr.ml parser.mli parser.ml lexer.mli lexer.ml printast.mli printast.ml pprintast.mli pprintast.ml main.ml

opt:
	ocamlyacc parser.mly
	ocamllex lexer.mll
	ocamlopt terminfo.mli terminfo.ml warnings.mli warnings.ml location.mli location.ml asttypes.mli misc.mli misc.ml longident.mli longident.ml parsetree.mli ast_helper.mli ast_helper.ml config.mli config.ml clflags.mli clflags.ml syntaxerr.mli syntaxerr.ml parser.mli parser.ml lexer.mli lexer.ml main.ml

clean:
	rm *.cm* *.o parser.ml lexer.ml a.out

