%{
#include <stdio.h>
#include "ast.h"

void yyerror(char *s) {
  printf("%s\n",s);
}

// #define YYDEBUG 1

#define YYERROR_VERBOSE 1

%}

%union {
   E   *node;
   int val;
   char* str;
}

%token NUMBER SYMBOL
%token GT LT GE LE EQ ASSIGN
%token PLUS MINUS MULT DIV
%token LPAREN RPAREN LBRACK RBRACK EOL ERR
%token COMMA SEMI COLONCOLON
%token FUN DEF ARROW
%token STRING


%right SEMI
%right ASSIGN
%right COMMA
%right COLONCOLON
%left  GT LT GE LE EQ
%left  MINUS PLUS
%left  MULT DIV
%left  NEG

%type  <node> exp
%type  <node> exps
%type  <node> params
%type  <val>  NUMBER
%type  <str> SYMBOL
%type  <str> STRING

%%
line:   |                        { setE(EUni); }
        | exps                    { if($1->tl==EUni) setE($1->hd); else setE(EMsg0(ESym("block"),$1));  }

exp:    | NUMBER                 { $$ = EInt($1); }
        | SYMBOL                 { $$ = ESym($1); }
        | STRING                 { $$ = EStr($1); }
        | DEF SYMBOL LPAREN params RPAREN ASSIGN exp { $$ = EDef($2, $4, $7); }
        | DEF {$$ = ESym("def"); }
        | FUN LPAREN params RPAREN ARROW exp { $$ = ELam($3,$6); }
        | exp PLUS  exp          { $$ = EBin("add", $1, $3);}
        | exp MINUS exp          { $$ = EBin("sub", $1, $3);}
        | exp MULT  exp          { $$ = EBin("mul", $1, $3);}
        | exp DIV   exp          { $$ = EBin("div", $1, $3);}
        | exp GT   exp           { $$ = EBin("gt", $1, $3);}
        | exp LT   exp           { $$ = EBin("lt", $1, $3);}
        | exp GE   exp           { $$ = EBin("ge", $1, $3);}
        | exp LE   exp           { $$ = EBin("le", $1, $3);}
        | exp EQ   exp           { $$ = EBin("eq", $1, $3);}
        | exp ASSIGN exp         { $$ = EBin("let", $1, $3);}
        | exp COLONCOLON exp     { $$ = EPair($1, $3); }
        | exp LPAREN params RPAREN { $$ = EMsg0($1, $3); }
        | MINUS  exp %prec NEG   { $$ = EBin("neg", $2, EUni);}
        | LPAREN exp RPAREN      { $$ = $2; }
        | LBRACK exps RBRACK     { $$ = $2; }
        | LBRACK RBRACK          { $$ = EUni; }
params: | exp                    { $$ = EPair($1, EUni); }
        | exp COMMA params       { $$ = EPair($1, $3) }

exps:   | exp                    { $$ = EPair($1, EUni); }
        | exp SEMI exps          { $$ = EPair($1, $3) }


%%
