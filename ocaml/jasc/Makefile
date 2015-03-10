EXE=jasc
OCAMLOPT=ocamlfind ocamlopt -I ext
OCAMLC=ocamlfind ocamlc -I ext
SRC=jData.ml jPPData.ml jReader.ml jWriter.ml jCode.ml jPPCode.ml jCodeReader.ml  jCodeWriter.ml
EXT= ext/enum.mli ext/enum.ml ext/extString.mli ext/extString.ml ext/IO.mli ext/IO.ml ext/extList.mli ext/extList.ml ext/pMap.mli ext/pMap.ml
ifeq ($(OS),Windows_NT)
	EXE=jasc.exe
endif

all: run2

$(EXE): parser.mly lexer.mll jasc.ml

run2:
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex lexer.mll
	$(OCAMLOPT) $(EXT) $(SRC) parser.ml lexer.ml jasc.ml -o $(EXE)
	javac -g:none -target 1.5 -source 1.5 a.java
	jasmd a > a.j
	cp a.class a_org.txt
	./$(EXE) a.j
	cp a.class a_dst.txt

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

push: clean
	git commit -a
	git push
pull:
	git pull

clean:
	rm -rf main *.cm* ext/*.cm* ext/*.o $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cmo) *.class
distclean: clean
	rm -rf $(EXE)

Makefile: ;
$(SRC): ;
