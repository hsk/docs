%{
open E

%}

%token <int> INT
%token <string> IDENT
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token ADD
%token EOF

%right prec_app
%left ADD

%type <E.t> exp
%start exp

%%

simple_exp:
| IDENT { Var($1) }
| LPAREN exp RPAREN { $2 }
exps:
|  { [] }
| exp exps { $1::$2 }
exp:
| simple_exp { $1 }
| exp LPAREN exps RPAREN %prec prec_app { App($1, $3) }
| exp LBRACE exps RBRACE %prec prec_app { App($1, $3) }
| exp ADD exp { Bin($1, "+", $3) }
| error
    { failwith
    (Printf.sprintf "parse error near characters %d-%d"
       (Parsing.symbol_start ())
       (Parsing.symbol_end ())) }
