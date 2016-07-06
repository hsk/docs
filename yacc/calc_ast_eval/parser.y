%{
#include "syntax.h"
#include <stdio.h>
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
  syntax* syntax;
}

%type <syntax> expr primary
%type <dbl> DOUBLE

%token DOUBLE ERROR

%left  '+' '-'
%left  '*' '/'

%%
program         : expr            { state.lval = $1; }
expr            : expr '+' expr   { $$ = syntax_bin_new($1, '+', $3); }
                | expr '-' expr   { $$ = syntax_bin_new($1, '-', $3); }
                | expr '*' expr   { $$ = syntax_bin_new($1, '*', $3); }
                | expr '/' expr   { $$ = syntax_bin_new($1, '/', $3); }
                | primary
primary         : DOUBLE          { $$ = syntax_double_new($1); }
                | '(' expr ')'    { $$ = $2; }
