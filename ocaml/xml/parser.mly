%{
    open Ast
%}

%token <string*(Ast.e*Ast.e)list> XML_START
%token <string> XML_STOP
%token <string*(Ast.e*Ast.e)list> XML_SINGLE
%token <string> XML_STR
%token <string> XML_COMMENT
%token <string> XML_CDATA
%token <Ast.e> XML_EXP

%token EOF
%token ADD SUB MUL DIV MOD
%token LPAREN RPAREN
%token LBRACE RBRACE
%token <int> INT
%token <Ast.xml> LT
%token SEMI COMMA
%token <string> ID

%start main
%type <Ast.xml> main

%start xml_tag
%type <Ast.xml> xml_tag

%start exp
%type <Ast.e> exp
%start exp_rparen
%type <Ast.e> exp_rparen

%%

/* exp */
exp_rparen  : | add RBRACE { $1 }
exp         : | add { $1 }
              | exp LPAREN comma_exps RPAREN { EApp($1, $3) }
              | exp LBRACE exps RBRACE { EApp($1, [EBlock $3]) }

add         : | mul ADD add { EBin($1,"+",$3) }
              | mul SUB add { EBin($1,"-",$3) }
              | mul { $1 }

mul         : | sexp MUL mul { EBin($1, "*", $3) }
              | sexp DIV mul { EBin($1, "/", $3) }
              | sexp MOD mul { EBin($1, "%", $3) }
              | sexp { $1 }

lt          : | LT { EXML $1 }

sexp        : | INT { EInt $1 }
              | LPAREN comma_exps RPAREN { ETuple $2 }
              | lt { $1 }
              | LBRACE exps RBRACE { EBlock $2 }
              | ID { EId $1 }
exps        : | { [] }
              | exp { [$1] }
              | exp SEMI exps { $1::$3 }

comma_exps  : | { [] }
              | exp { [$1] }
              | exp COMMA comma_exps { $1::$3 }

/* xml */
main        : | xml_tag EOF { $1 }
xml_tag     : | XML_START values XML_STOP {
                  let (a, ls) = $1 in
                  if a <> $3 then failwith "end tag error";
                  XmlTag(a, ls, $2)
                }
              | XML_SINGLE { XmlSingle $1 }
values      : | { [] }
              | value values { $1::$2 }
value       : | xml_tag { $1 }
              | XML_STR { XmlText $1 }
              | XML_COMMENT { XmlComment $1 }
              | XML_CDATA { XmlCData $1 }
              | XML_EXP { XmlExp $1 }
