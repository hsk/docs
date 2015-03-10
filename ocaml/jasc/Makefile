OCAMLOPT=ocamlfind ocamlopt -I ext -package ppx_deriving.show -linkpkg
OCAMLC=ocamlfind ocamlc -I ext -package ppx_deriving.show -linkpkg
SRC=jData.ml jReader.ml jWriter.ml jCode.ml jCodeReader.ml  jCodeWriter.ml
EXT= ext/enum.mli ext/enum.ml ext/extString.mli ext/extString.ml ext/IO.mli ext/IO.ml ext/extList.mli ext/extList.ml ext/pMap.mli ext/pMap.ml
all: bytecode native jasc

jasc: parser.mly lexer.mll jasc.ml
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex lexer.mll
	$(OCAMLOPT) -package ppx_deriving.show -linkpkg $(EXT) $(SRC) parser.ml lexer.ml jasc.ml -o jasc


run:
	$(OCAMLOPT) -package ppx_deriving.show -linkpkg $(EXT) $(SRC) main.ml -o main
	javac a.java
	#jasmd a -high
	cp a.class a_org.txt
	#java a
	#java -jar Jasper.jar a.class

	#rm a.class
	./main
	cp a.class a_dst.txt
	jasmd a

install:
	wget http://www.angelfire.com/tx4/cus/jasper/jasper.zip
	unzip jasper.zip



clean:
	rm -rf main jasc *.cm* ext/*.cm* ext/*.o $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cmo) *.class
.PHONY: all bytecode native clean

Makefile: ;
$(SRC): ;
