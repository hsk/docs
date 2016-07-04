all :
	bison -y -d -o parser.c parser.y
	flex --header-file=lexer.h -olexer.c lexer.l
	gcc parser.c lexer.c main.c	-o calc

clean :
	rm -f parser.c parser.h lexer.c lexer.h *.o calc
