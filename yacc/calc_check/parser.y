%{
#include <stdio.h>
#define YYDEBUG 1
#define YYERROR_VERBOSE 1
static void
yyerror(const char *s) {
  fprintf(stderr, "%s\n", s);
}
int yylex();
%}

%token DOUBLE
%token ERROR

%left  '+' '-'
%left  '*' '/'

%%
program         : expr
expr            : expr '+' expr
                | expr '-' expr
                | expr '*' expr
                | expr '/' expr
                | primary
primary         : DOUBLE
                | '(' expr ')'
