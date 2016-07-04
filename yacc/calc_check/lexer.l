%{
#include "parser.h"
#include <string.h>

#define YY_DECL int yylex()
%}
%option noyywrap
%%

[ \t]                             { }
"+"                               { return '+'; }
"-"                               { return '-'; }
"*"                               { return '*'; }
"/"                               { return '/'; }
"("                               { return '('; }
")"                               { return ')'; }
(([1-9][0-9]*)|0)(\.[0-9][0-9]*)? { return DOUBLE; }
.                                 {
                                    char c = yytext[0];
                                    fprintf(stderr, "lexical error near '%c'\n", c);
                                    return ERROR;
                                  }
