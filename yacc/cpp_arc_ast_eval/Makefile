all:
	bison -y --defines=parser.h -o parser.cpp parser.y
	flex --header-file=lexer.h -o lexer.cpp lexer.l
	g++ parser.cpp lexer.cpp main.cpp -o calc

clean:
	rm -f parser.cpp parser.h lexer.cpp lexer.h *.o calc
