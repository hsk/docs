all: http

dylib: dylib.c dylib_main.c
	gcc -shared -fPIC -o dylib.so dylib.c
	gcc -o dylib dylib_main.c dylib.so
	./dylib

dyload:
	gcc -shared -fPIC -o dylib.so dylib.c
	gcc -rdynamic -o dyload dyload.c -ldl

dygc: gc.c test.c dygc.c test1.c
	gcc -shared -fPIC -o gc.so gc.c
	gcc -shared -fPIC -o test.so test.c gc.so
	gcc -rdynamic -shared -fPIC -o test1.so test1.c
	gcc -rdynamic -o dygc dygc.c -ldl gc.so
	./dygc

http: http.c gc.so
	gcc -o http http.c -ldl gc.so -lc
	open http://localhost:8088/index.html &
	./http

gc.so: gc.c
	gcc -shared -fPIC -o gc.so gc.c

clean:
	rm -rf a.out dylib *.so dygc dyload http
