all: gc1 gc2 gc3 gc4

gc1:
	ocaml test1.ml > test1.c
	gcc test1.c -o test1
	./test1

gc2: gc.h gc.c test2.c
	gcc gc.c test2.c -o test2
	./test2

gc3:
	ocaml test3.ml > test3.c
	gcc gc.c test3.c -o test3
	./test3

gc4:
	ocaml test4.ml > test4.c
	gcc gc.c test4.c -o test4
	./test4

