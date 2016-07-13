%{
open Ast
%}
%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS
%start main
%type <Ast.e> main
%%

main: | expr EOL                 { $1 }
expr: | INT                      { EInt $1 }
      | LPAREN expr RPAREN       { $2 }
      | expr PLUS expr           { EBin ($1, "add", $3) }
      | expr MINUS expr          { EBin ($1, "sub", $3) }
      | expr TIMES expr          { EBin ($1, "mul", $3) }
      | expr DIV expr            { EBin ($1, "div", $3) }
      | MINUS expr %prec UMINUS  { EBin (EInt 0, "sub", $2) }
