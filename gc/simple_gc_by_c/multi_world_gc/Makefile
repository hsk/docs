all: gctest test

gctest: gc.c
	gcc gc.c -o gctest

test: gctest
	./gctest

clean:
	rm -f gctest a.out
