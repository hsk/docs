%{
#include "syntax.h"
#include <stdio.h>

#define YYDEBUG 1
#define YYERROR_VERBOSE 1
%}
%union {
  double dbl;
}
%{
static void
yyerror(parser_state *p, const char *s) {
    fprintf(stderr, "%s\n", s);
}
int yylex(YYSTYPE *lval, parser_state *p);
%}
%pure-parser
%parse-param { parser_state *p }
%lex-param { p }

%type <dbl> expr primary
%type <dbl> DOUBLE

%token DOUBLE ERROR
%left  '+' '-'
%left  '*' '/'

%%
program         : expr            { p->lval = $1; }
expr            : expr '+' expr   { $$ = $1 + $3; }
                | expr '-' expr   { $$ = $1 - $3; }
                | expr '*' expr   { $$ = $1 * $3; }
                | expr '/' expr   { $$ = $1 / $3; }
                | primary
primary         : DOUBLE
                | '(' expr ')'    { $$ = $2; }
