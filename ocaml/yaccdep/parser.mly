%{
open Ast
let lineno = ref 0
let parse_error1 lineno = 
  Printf.sprintf
    ("\n＿人人 人人＿\n＞ 突然の死 ＜ parse error line %d\n￣Y^Y^Y^Y^Y￣\n")
    lineno
let parse_error2 str = 
  Printf.sprintf
    ("\n＿人人 人人＿\n＞ 突然の死 ＜ %s\n￣Y^Y^Y^Y^Y￣\n")
    str
%}

%token <int> INT
%token <string> FLOAT
%token <string> VAR
%token <string> STR
%token <string> CHR
%token <string list> STARTS
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token EOF
%token COLON
%token OR SEMI
%right LIST
%left OR

%type <string list * Ast.rule list> prog
%start prog

%%

e:
  | INT { [] }
  | FLOAT { [] }
  | VAR { [] }
  | STR { [] }
  | CHR { [] }
  | COLON { [] }
  | SEMI { [] }
  | OR { [] }
  | LPAREN es RPAREN { $2 }
  | LBRACE es RBRACE { $2 }
  | LBRACK es RBRACK { $2 }
  ;

es:
  |      { [] }
  | e es { [] }

prog:
  | STARTS rules { ($1,$2) }

rules:
  | rule { [$1] }
  | rule rules { $1::$2 }
semi:
  | { [] }
  | SEMI { [] }
rule:
  | VAR COLON ptns semi           { Rule($1,$3) }
ptns:
  | ptn                      { [$1] }
  | ptn ptns                 { $1::$2 }
ptn:
  | OR vars LBRACE es RBRACE { Ptn($2) }
vars:
  |                          { [] }
  | VAR vars                 { $1::$2 }

functor_arg:
  | LPAREN RPAREN
      { [] }
  ;

functor_arg_name:
  | SEMI     { [] }
  | VAR { [] }
  ;


functor_args:
  | functor_args functor_arg
      { [] }
  | functor_arg
      { [] }
  ;
