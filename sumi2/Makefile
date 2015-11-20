all : expr
	./expr a.txt
expr : parser.tab.c lex.yy.c ast.c eval.c main.c
	gcc -o $@ parser.tab.c lex.yy.c ast.c eval.c main.c

lex.yy.c : lexer.l parser.tab.h
	flex lexer.l

parser.tab.c : parser.y
	bison -dv parser.y

clean :
	rm -rf *.tab.* *.yy.c *.output expr *.o
