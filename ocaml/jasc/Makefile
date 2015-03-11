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

all: run

$(EXE): parser.mly lexer.mll jasc.ml

run:
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex lexer.mll
	$(OCAMLOPT) $(EXT) $(SRC) parser.ml lexer.ml jasc.ml -o $(EXE)

test:

#	$(EXE) tests/InvokeDynamic.j
	$(EXE) tests/Test001.j
	$(EXE) tests/Test002.j
	$(EXE) tests/Test003.j
	$(EXE) tests/Test004.j
	$(EXE) tests/Test005.j
	$(EXE) tests/Test006.j
	$(EXE) tests/Test007.j
	$(EXE) tests/Test008.j
	$(EXE) tests/Test009.j
	$(EXE) tests/Test010.j
	$(EXE) tests/Test_b.j
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

#	java tests.InvokeDynamic
#	java tests.Test001
#	java tests.Test002
	java tests.Test003
	java tests.Test004
	java tests.Test005
	java tests.Test006 aaa
	java tests.Test007
	java tests.Test008
	java tests.Test009
	java tests.Test010
	java tests.Test_b
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
	cp jasc /usr/local/bin/

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
