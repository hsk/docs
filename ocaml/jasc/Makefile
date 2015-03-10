EXE=jasc
OCAMLOPT=ocamlfind ocamlopt -I ext -g
OCAMLC=ocamlfind ocamlc -I ext -g
SRC=jData.ml jPPData.ml jReader.ml jWriter.ml jCode.ml jPPCode.ml jCodeReader.ml  jCodeWriter.ml
EXT= ext/enum.mli ext/enum.ml ext/extString.mli ext/extString.ml ext/IO.mli ext/IO.ml ext/extList.mli ext/extList.ml ext/pMap.mli ext/pMap.ml
ifeq ($(OS),Windows_NT)
	EXE=jasc.exe
else
	EXE=./jasc
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
	javap -v a > a_org.p
	cp a.class a_org.txt
	./$(EXE) a.j
	cp a.class a_dst.txt
	javap -v a > a_dst.p
	diff a_org.p a_dst.p
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

test:

#	$(EXE) tests/InvokeDynamic.j
#	java tests.InvokeDynamic
	$(EXE) tests/Test001.j
#	java tests.Test001
	$(EXE) tests/Test002.j
#	java tests.Test002
	$(EXE) tests/Test003.j
	java tests.Test003
	$(EXE) tests/Test004.j
	java tests.Test004
	$(EXE) tests/Test005.j
	java tests.Test005
	$(EXE) tests/Test006.j
	java tests.Test006 aaa
	$(EXE) tests/Test007.j
	java tests.Test007
	$(EXE) tests/Test008.j
	java tests.Test008
	$(EXE) tests/Test009.j
	java tests.Test009
	$(EXE) tests/Test010.j
	java tests.Test010
	$(EXE) tests/Test_b.j
	java tests.Test_b
	$(EXE) tests/Test_c.j
	$(EXE) tests/Test_d.j
	$(EXE) tests/Test_f.j
	$(EXE) tests/Test_g.j
	$(EXE) tests/Test_i.j
	$(EXE) tests/Test_interface.j
	$(EXE) tests/Test_interface2.j
	$(EXE) tests/Test_l.j
	$(EXE) tests/Test_m.j
	$(EXE) tests/Test_s.j
	$(EXE) tests/Test_switch.j
	$(EXE) tests/Test_switch2.j
	$(EXE) tests/Test_switch3.j
	$(EXE) tests/Test_throws.j
	$(EXE) tests/Test_while.j
	$(EXE) tests/jsr.j
	$(EXE) tests/jsr2.j

	java tests.Test_c
	java tests.Test_d
	java tests.Test_f
	java tests.Test_g
	java tests.Test_i
#	java tests.Test_interface
	java tests.Test_interface2
	java tests.Test_l
	java tests.Test_m
	java tests.Test_s
	java tests.Test_switch
	java tests.Test_switch2
	java tests.Test_switch3
#	java tests.Test_throws
	java tests.Test_while
	java tests.jsr
	java tests.jsr2


install:
	wget http://www.angelfire.com/tx4/cus/jasper/jasper.zip
	unzip jasper.zip

push: clean
	git commit -a
	git push
pull:
	git pull

clean:
	rm -rf main lexer.ml parser.ml *.cm* ext/*.cm* ext/*.o $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cmo) *.class
distclean: clean
	rm -rf $(EXE)

Makefile: ;
$(SRC): ;
