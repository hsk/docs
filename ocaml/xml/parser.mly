%{
	open Ast
	let rec attrs = function
	  | [] -> ""
	  | (k,v)::xs -> Printf.sprintf " %s=%S" k v ^ attrs xs
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
%token RBRACE
%token <int> INT
%token <Ast.xml> LT

%start main
%type <Ast.xml> main

%start xml_tag
%type <Ast.xml> xml_tag

%start exp
%type <Ast.e> exp
%start exp_rparen
%type <Ast.e> exp_rparen

%%

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

/* exp */
exp_rparen  : | add RBRACE { $1 }
exp         : | add { $1 }

add         : | mul ADD add { EBin($1,"+",$3) }
              | mul SUB add { EBin($1,"-",$3) }
              | mul { $1 }

mul         : | sexp MUL mul { EBin($1, "*", $3) }
              | sexp DIV mul { EBin($1, "/", $3) }
              | sexp MOD mul { EBin($1, "%", $3) }
              | sexp { $1 }

lt          : | LT { EXML $1 }

sexp        : | INT { EInt $1 }
              | LPAREN exp RPAREN { $2 }
              | lt { $1 }
