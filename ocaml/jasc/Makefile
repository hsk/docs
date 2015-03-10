EXE=jasc
OCAMLOPT=ocamlfind ocamlopt -I ext
OCAMLC=ocamlfind ocamlc -I ext
SRC=jData.ml jReader.ml jWriter.ml jCode.ml jCodeReader.ml  jCodeWriter.ml
EXT= ext/enum.mli ext/enum.ml ext/extString.mli ext/extString.ml ext/IO.mli ext/IO.ml ext/extList.mli ext/extList.ml ext/pMap.mli ext/pMap.ml
ifeq ($(OS),Windows_NT)
	EXE=jasc.exe
else
	OCAMLOPT += " -package ppx_deriving.show"
endif
all: $(EXE)

$(EXE): parser.mly lexer.mll jasc.ml
ifeq ($(OS),Windows_NT)
	cp win/jCode.ml .
	cp win/jData.ml .
endif
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex lexer.mll
	$(OCAMLOPT) $(EXT) $(SRC) parser.ml lexer.ml jasc.ml -o $(EXE)


run:
	$(OCAMLOPT) $(EXT) $(SRC) main.ml -o main
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
