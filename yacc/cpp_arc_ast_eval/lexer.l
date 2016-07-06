%{
#include "syntax.h"
#include "parser.h"
#include <string.h>
#define YY_DECL int yylex()
%}
%option noyywrap
%option yylineno
%%

[ \t]                             { }
"+"                               { return PLUS; }
"-"                               { return MINUS; }
"*"                               { return MUL; }
"/"                               { return DIV; }
"("                               { return '('; }
")"                               { return ')'; }
(([1-9][0-9]*)|0)(\.[0-9][0-9]*)? { double d;
                                    sscanf(yytext, "%lf", &d);
                                    yylval.dbl = d;
                                    return DOUBLE;
                                  }
.                                 {
                                    char c = yytext[0];
                                    fprintf(stderr, "lexical error near '%c'\n", c);
                                    return ERROR;
                                  }
