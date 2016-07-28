CFLAGS = -std=c++14 -g
CC = clang++ $(CFLAGS)

all:
	bison --defines=parser.h -o parser.cpp parser.y
	flex -o lexer.cpp --header-file=lexer.h lexer.l 
	$(CC) -o calc -Wno-deprecated-register *.cpp
	rm -rf parser.cpp parser.h* lexer.h* lexer.cpp
	/usr/local/bin/valgrind --leak-check=full --leak-resolution=low ./calc
#	/usr/local/bin/valgrind --tool=memcheck --leak-check=yes --leak-resolution=high --num-callers=40 --undef-value-errors=no --run-libc-freeres=no -v ./calc

calc: *.cpp *.h *.l *.y
	make all

clean:
	rm -rf parser.cpp parser.h* lexer.h* lexer.cpp calc a.exe a.out a.s
