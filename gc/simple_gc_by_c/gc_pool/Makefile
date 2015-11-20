all: test test2

gctest: gc.c
	gcc gc.c -o gctest

gctest2: gc.c
	g++ gc.c -o gctest2

test: gctest
	time ./gctest

test2: gctest2
	time ./gctest2

clean:
	rm -f gctest gctest2 a.out
