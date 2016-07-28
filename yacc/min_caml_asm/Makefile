CFLAGS = -std=c++14 -g
CC = clang++ $(CFLAGS)

all:
	bison --defines=parser.h -o parser.cpp parser.y
	flex -o lexer.cpp --header-file=lexer.h lexer.l 
	$(CC) -o mincamlasm -Wno-deprecated-register *.cpp
	rm -rf parser.cpp parser.h* lexer.h* lexer.cpp
	/usr/local/bin/valgrind --leak-check=full --leak-resolution=low ./mincamlasm x86_test/fib
#	/usr/local/bin/valgrind --tool=memcheck --leak-check=yes --leak-resolution=high --num-callers=40 --undef-value-errors=no --run-libc-freeres=no -v ./mincamlasm

mincamlasm: *.cpp *.h *.l *.y
	make all

clean:
	rm -rf parser.cpp parser.h* lexer.h* lexer.cpp mincamlasm* a.exe a.out a.s x86_test/*.s x86_test/*.exe
