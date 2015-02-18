%{
open E

%}

%token <int> INT
%token <string> IDENT
%token <string>EQ
%token <string>LET
%token <string>IN
%token <string>COM
%token <string>FCOM
%token <string>MATCH
%token <string>WITH
%token <string>BAR
%token EOF

%right prec_let
%left EQ

%type <E.e> exp
%start exp

%%

simple_exp:
| IDENT { Tok($1) }
| COM { RCom($1) }

exps:
| exp { [$1] }
| exp exps { $1::$2 }
exp:
| FCOM exp { FCom($1, $2) }
| simple_exp { $1 }
| simple_exp exp { Cons($1, $2) }
| LET exps EQ exps IN exps %prec prec_let { Let(Tok $1 :: $2@[Tok $3], $4@[Tok "in"], $6) }
| MATCH exps WITH case %prec prec_let { Match(Tok $1 :: $2 @ [Tok "with"], $4) }
| MATCH exps WITH case %prec prec_let { Match(Tok $1 :: $2 @ [Tok "with"], $4) }
| error
    { failwith
    (Printf.sprintf "parse error near characters %d-%d"
       (Parsing.symbol_start ())
       (Parsing.symbol_end ())) }

case:
| exps { [$1] }
| exps cases { ($1)::$2 }
| cases { $1 }

cases:
| BAR exps { [Tok"|"::$2] }
| BAR exps cases { (Tok"|"::$2) :: $3 }
