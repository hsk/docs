%{
#include <stdio.h>
#include "ast.h"
#include "parser.tab.h"

int yywrap(void){ return 1;}

%}

%%

[0-9]+            {
                    yylval.val = atoi(yytext);
                    return(NUMBER);
                  }
"fun"      return(FUN);
"def"      return(DEF);
"->"       return(ARROW);
[a-zA-Z_][a-zA-Z_0-9]* { yylval.str = strdup(yytext); return(SYMBOL); }
"+"        return(PLUS);
"-"        return(MINUS);
"*"        return(MULT);
"/"        return(DIV);
">"        return(GT);
"<"        return(LT);
">="       return(GE);
"<="       return(LE);
"=="       return(EQ);
"="        return(ASSIGN);
"("        return(LPAREN);
")"        return(RPAREN);
"["        return(LBRACK);
"]"        return(RBRACK);
","        return(COMMA);
";"        return(SEMI);
\"([^\"]|\\.)*\"  { int len = strlen(yytext); yylval.str = malloc(len-2+1); memcpy(yylval.str, &yytext[1], len - 2); yylval.str[len-1]='\0'; return (STRING); }
" "        {}
"\n"       {}
.          { yyerror("Illegal character"); 
             return(ERR);
           }
%%