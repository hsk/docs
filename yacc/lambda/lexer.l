%{
#include "syntax.h"
#include "parser.h"
#include <string.h>

#define YY_DECL int yylex(YYSTYPE *lval, parser_state *p)
%}
%option noyywrap
digit [0-9]
alpha [a-zA-Z_]
ident {alpha}({alpha}|{digit})*
%%

[ \t\r\n]+                        { }
"+"                               { return '+'; }
"-"                               { return '-'; }
"*"                               { return '*'; }
"/"                               { return '/'; }
"("                               { return '('; }
")"                               { return ')'; }
"<="                              { return LE; }
"="                               { return '='; }
"::"                              { return CONS; }
"->"                              { return ARROW; }
"if"                              { return IF; }
"then"                            { return THEN; }
"else"                            { return ELSE; }
"let"                             { return LET; }
"rec"                             { return REC; }
"in"                              { return IN; }
{ident}                           { int len = strlen(yytext);
                                    lval->str = malloc(len+1);
                                    strcpy(lval->str,yytext);
                                    lval->str[len]='\0';
                                    return IDENT;
                                  }
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
