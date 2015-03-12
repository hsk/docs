EXE=jasc
OCAMLOPT=ocamlfind ocamlopt -I ext -g
OCAMLC=ocamlfind ocamlc -I ext -g
SRC=jData.ml jDataPP.ml jReader.ml jWriter.ml jCode.ml jCodePP.ml jCodeReader.ml  jCodeWriter.ml
EXT= ext/enum.mli ext/enum.ml ext/extString.mli ext/extString.ml ext/IO.mli ext/IO.ml ext/extList.mli ext/extList.ml ext/pMap.mli ext/pMap.ml
ifeq ($(OS),Windows_NT)
	EXE=jasc.exe
	INSTALL_PATH=/usr/bin/
else
	EXE=./jasc
	INSTALL_PATH=/usr/local/bin/
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
	$(EXE) tests/*.j examples/*.j
	java examples.ANewArray
#	java examples.AnInterface
	java examples.Arrays a
	java examples.Catch
	java examples.Checkcast
	java examples.Count
#	java examples.HelloWorld # error ok
	java examples.Implementor
	java examples.InvokeInterface
	java examples.MultiANewArray
	java examples.NewArray
	java examples.Switch
#	java examples.Uncaught # exception ok
#	java examples.VerifyTest # error ok
#	java examples.VerifyTest1 # error ok

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
	cp jasc $(INSTALL_PATH)

push: clean
	git commit -a
	git push
pull:
	git pull

clean:
	rm -rf main tests/*.class lexer.ml parser.ml *.cm* ext/*.cm* ext/*.o $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cmo) *.class
distclean: clean
	rm -rf $(EXE)

Makefile: ;
$(SRC): ;
