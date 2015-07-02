all: main test
main: peg_dats.o syntax_dats.o parser_dats.o main.dats
	patscc -DATS_MEMALLOC_LIBC -L$(PATSHOME)/ccomp/atslib/lib -latslib peg_dats.o syntax_dats.o parser_dats.o main.dats -o main
test: main
	./main a.txt

peg_dats.o: peg.sats peg.hats peg.dats
	patscc -DATS_MEMALLOC_LIBC peg.dats -c
syntax_dats.o: syntax.sats syntax.hats syntax.dats
	patscc -DATS_MEMALLOC_LIBC syntax.dats -c
parser_dats.o: parser.sats parser.hats parser.dats
	patscc -DATS_MEMALLOC_LIBC parser.dats -c

clean:
	rm -rf *.c *.o main

