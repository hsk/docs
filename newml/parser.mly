%{
open Ast

%}

%token <int> INT
%token <string> ID
%token SUB ADD
%token OR
%token SEMICOLON
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token PRINT
%token EOF
%token COLON COMMA COLONASSIGN
%token ASSIGN
%token RETURN
%token <string> OPEN
%token <string> STRING
%token STRUCT THIS DOT
%token IF ELSE
%token IMPLEMENT RIMPLEMENT TRAIT
%token ARROW MEMBER FARROW
%token LT GT LE GE
%token MUL AMP DIV
%token CAST NEW AT DEF CASE MATCH TYPE

%right LIST
%nonassoc ELSE
%right ASSIGN COLONASSIGN
%left COMMA
%right CAST
%left LT GT LE GE
%left ADD SUB
%left MUL DIV
%left NEW
%left prec_app
%left MEMBER
%left DOT
%right ARROW
%left AT
%left LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%left CALL

%type <Ast.prog> prog
%start prog

%%


typ:
| ID { Ty $1 }
| typ ARROW typ { TFun($1, $3) }
| LPAREN typs RPAREN { TTuple($2)}
| LPAREN RPAREN { TUnit }
typs:
| typ { [$1]}
| typ COMMA typs { $1 :: $3 }

exp1:
| exp { $1 }
| exp SEMICOLON { $1 }

exps:
| exp1 { [$1] }
| exp1 exps %prec LIST { $1 :: $2 }


defrec:
|   ID COLON typ {($1, $3) }

defrecs:
|   defrec { [$1] }
|   defrec SEMICOLON defrecs { $1::$3 }

record:
| ID ASSIGN exp { ($1, $3) }
| ID { ($1, EEmpty)}
records:
| record { [$1] }
| record SEMICOLON records { $1::$3 }

variant:
| ID typ { ($1,$2) }
| ID { ($1,TEmpty) }

variants:
| variant { [$1] }
| variant OR variants { $1::$3 }

exp:
| ID TYPE LBRACE SEMICOLON defrecs RBRACE { ETypeRec($1, $5)}
| ID TYPE OR variants { ETypeVariant($1, $4)}
| SUB exp { EPre("-", $2)}
| exp ADD exp { EBin($1, "+", $3) }
| exp SUB exp { EBin($1, "-", $3) }
| exp MUL exp { EBin($1, "*", $3) }
| exp DIV exp { EBin($1, "/", $3) }
| exp DOT exp { EBin($1, ".", $3) }
| exp LT exp { EBin($1, "<", $3) }
| exp GT exp { EBin($1, ">", $3) }
| exp LE exp { EBin($1, "<=", $3) }
| exp GE exp { EBin($1, ">=", $3) }
| exp OR exp { EBin($1, "lor", $3) }
| exp COMMA exp { EBin($1, ",", $3) }
| exp MATCH LBRACE CASE fns RBRACE { EMatch($1, $5) }
| IF LPAREN exp RPAREN exp ELSE exp { EIf($3,$5,$7) }
| IF LPAREN exp RPAREN exp %prec LIST { EIf($3,$5,EEmpty) }
| LBRACE fn RBRACE { $2 }
| LBRACE CASE fns RBRACE { EPFun($3) }
| LBRACE exps RBRACE { EBlock($2) }
| LBRACK RBRACK { EList[]}
| LBRACK exps RBRACK { EList $2 }
| LPAREN RPAREN { EUnit }
| LPAREN exp RPAREN { $2 }
| LBRACE SEMICOLON records RBRACE { ERecord($3) }
| exp LBRACE fn RBRACE %prec CALL { ECall($1, [$3]) }
| exp LBRACE CASE fns RBRACE %prec CALL { ECall($1, [EPFun($4)]) }
| exp LBRACE exps RBRACE %prec CALL { ECall($1, [EBlock($3)]) }
| exp LBRACK RBRACK %prec CALL { ECall($1, [EList[]]) }
| exp LBRACK exps RBRACK %prec CALL { ECall($1, [EList $3]) }
| exp LPAREN exps RPAREN %prec CALL { ECall($1, $3) }
| exp LPAREN RPAREN %prec CALL { ECall($1, [EUnit]) }
| INT { EInt($1) }
| ID { EVar($1) }
| STRING { EString($1) }
| ID COLONASSIGN exp { ELet($1, TEmpty, $3) }
| DEF ID COLONASSIGN exp { ELetRec($2, TEmpty, $4) }
| ID COLON typ ASSIGN exp { ELet($1, $3, $5) }
| DEF ID COLON typ ASSIGN exp { ELetRec($2, $4, $6) }
| exp MEMBER exp { ECall($3, [$1]) }
fn:
| exps ARROW exps { EFun($1, TEmpty, EBlock($3)) }

fns:
| fn { [$1] }
| fn CASE fns { $1 :: $3 }
stmt:
| exp { SExp($1) }
| ID COLONASSIGN exp { SLet($1, TEmpty, $3) }
| DEF ID COLONASSIGN exp { SLetRec($2, TEmpty, $4) }
| OPEN { SOpen($1) }
stmts:
| stmt { [$1] }
| stmt stmts { $1 :: $2 }

prog:
| stmts { Prog $1 }

| error
    { failwith
      (Printf.sprintf "parse error near characters %d-%d"
        (Parsing.symbol_start ())
        (Parsing.symbol_end ())) }
