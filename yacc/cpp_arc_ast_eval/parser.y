%{
#include <stdio.h>
#include "syntax.h"
#define YYDEBUG 1
#define YYERROR_VERBOSE 1

static void
yyerror(const char *s) {
  fprintf(stderr, "%s\n", s);
}
int yylex();
%}
%union {
  double dbl;
  E* syntax;
}

%type <syntax> expr primary
%type <dbl> DOUBLE

%token PLUS MINUS MUL DIV
%token DOUBLE ERROR

%left  PLUS MINUS
%left  MUL DIV

%%
program         : expr            { state.lval = $1; }
expr            : expr PLUS expr  { $$ = new EAdd($1, $3); }
                | expr MINUS expr { $$ = new ESub($1, $3); }
                | expr MUL expr   { $$ = new EMul($1, $3); }
                | expr DIV expr   { $$ = new EDiv($1, $3); }
                | primary
primary         : DOUBLE          { $$ = new EDbl($1); }
                | '(' expr ')'    { $$ = $2; }
