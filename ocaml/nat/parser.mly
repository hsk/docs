%token S Z
%token LPAREN RPAREN
%token EOL

%start main
%type <Ast.n> main

%%

main:
  | n EOL { $1 }

n:
  | Z                   { Ast.Z }
  | S LPAREN n RPAREN   { Ast.S( $3 ) }
