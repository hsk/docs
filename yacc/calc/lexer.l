%{
#include "syntax.h"
#include "parser.h"
#include <string.h>

#define YY_DECL int yylex(YYSTYPE *lval, parser_state *p)

%}

%option noyywrap

%%
[ \t]                             { }
"+"                               { return '+'; }
"-"                               { return '-'; }
"*"                               { return '*'; }
"/"                               { return '/'; }
(([1-9][0-9]*)|0)(\.[0-9][0-9]*)? { double d;
                                    sscanf(yytext, "%lf", &d);
                                    lval->dbl = d;
                                    return DOUBLE;
                                  }
.                                 {
                                    char c = yytext[0];
                                    fprintf(stderr, "lexical error near '%c'\n", c);
                                    return ERROR;
                                  }
