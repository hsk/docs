%{
#include "syntax.h"
#include <stdio.h>
#define YYDEBUG 1
#define YYERROR_VERBOSE 1
%}
%union {
  double dbl;
  syntax* syntax;
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

%type <syntax> expr primary
%type <dbl> DOUBLE

%token PLUS MINUS MUL DIV
%token DOUBLE

%left  PLUS MINUS
%left  MUL DIV

%%
program         : expr            { p->lval = $1; }
expr            : expr PLUS expr  { $$ = syntax_bin_new($1, "+", $3); }
                | expr MINUS expr { $$ = syntax_bin_new($1, "-", $3); }
                | expr MUL expr   { $$ = syntax_bin_new($1, "*", $3); }
                | expr DIV expr   { $$ = syntax_bin_new($1, "/", $3); }
                | primary
primary         : DOUBLE          { $$ = syntax_double_new($1); }
                | '(' expr ')'    { $$ = $2; }
