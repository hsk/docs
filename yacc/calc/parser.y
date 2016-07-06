%{
#include "syntax.h"
#include <stdio.h>
#include "lexer.h"

#define YYDEBUG 1
#define YYERROR_VERBOSE 1

static void
yyerror(const char *s) {
    fprintf(stderr, "%s\n", s);
}
int yylex();
parser_state state;
%}
%union {
  double dbl;
}

%type <dbl> expr primary
%type <dbl> DOUBLE

%token DOUBLE ERROR
%left  '+' '-'
%left  '*' '/'

%%
program         : expr            { /*printf("line=%d\n", yylineno);*/ state.lval = $1; }
expr            : expr '+' expr   { $$ = $1 + $3; }
                | expr '-' expr   { $$ = $1 - $3; }
                | expr '*' expr   { $$ = $1 * $3; }
                | expr '/' expr   { $$ = $1 / $3; }
                | primary
primary         : DOUBLE
                | '(' expr ')'    { $$ = $2; }
