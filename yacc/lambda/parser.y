%{
#include "syntax.h"
#include <stdio.h>
#define YYDEBUG 1
#define YYERROR_VERBOSE 1
%}
%union {
  double dbl;
  syntax* syntax;
  char* str;
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

%type <syntax> expr primary_expr prefix_expr infix_expr control_expr
%type <dbl> DOUBLE
%type <str> IDENT

%token DOUBLE ERROR
%token IDENT
%token IF THEN ELSE LET REC IN APP CONS

%right CONS
%right APP
%right ARROW
%left  LE
%left  '+' '-'
%left  '*' '/'

%%
program         : expr                           { p->lval = $1; }
expr            : infix_expr                     { $$ = $1; }
                | prefix_expr                    { $$ = $1; }
                | primary_expr                   { $$ = $1; }
                | control_expr                   { $$ = $1; }
infix_expr      : expr CONS expr                 { $$ = syntax_bin_new($1, CONS, $3); }
                | expr LE  expr                  { $$ = syntax_bin_new($1, LE, $3); }
                | expr '+' expr                  { $$ = syntax_bin_new($1, '+', $3); }
                | expr '-' expr                  { $$ = syntax_bin_new($1, '-', $3); }
                | expr '*' expr                  { $$ = syntax_bin_new($1, '*', $3); }
                | expr '/' expr                  { $$ = syntax_bin_new($1, '/', $3); }
                | expr ARROW expr                { $$ = syntax_bin_new($1, ARROW, $3); }
prefix_expr     : '+' expr                       { $$ = syntax_pre_new('+', $2); }
                | '-' expr                       { $$ = syntax_pre_new('-', $2); }
control_expr    : IF expr THEN expr ELSE expr    { $$ = syntax_if_new($2, $4, $6); }
                | LET IDENT '=' expr IN expr     { $$ = syntax_let_new(0, $2, $4, $6); free($2); }
                | LET REC IDENT '=' expr IN expr { $$ = syntax_let_new(1, $3, $5, $7); free($3); }
                | expr expr     %prec APP        { $$ = syntax_bin_new($1, APP, $2); }
primary_expr    : DOUBLE                         { $$ = syntax_double_new($1); }
                | IDENT                          { $$ = syntax_var_new($1); free($1); }
                | '(' ')'                        { $$ = syntax_nil; }
                | '(' expr ')'                   { $$ = $2; }
