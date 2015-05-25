{
open Parser
}

let space = [' ' '\t']
let identchar = ['A'-'Z' 'a'-'z' '_' '0'-'9' ':' '-']

rule token = parse
  | space+ { token lexbuf }
  | ['0' - '9']+ as s { INT (int_of_string s) }
  | '+' { ADD }
  | '-' { SUB }
  | '*' { MUL }
  | '/' { DIV }
  | '%' { MOD }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '<' {  lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
    LT (Parser.xml_tag xml_token lexbuf) }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ';' { SEMI }
  | ',' { COMMA }
  | ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '_' '0'-'9']* as s { ID s }
  | eof { EOF }

and xml_token = parse
  | '<' (['a'-'z'] ['a'-'z' '0'-'9']* as s) space*
    {
      match xml_attributes lexbuf with
      | l,false -> XML_START(s, l)
      | l,true -> XML_SINGLE(s, l)
    }
  | "</" (['a'-'z'] ['a'-'z' '0'-'9']* as s) '>' { XML_STOP s }
  | "<!--" as s { XML_COMMENT (s ^ xml_comment lexbuf) }
  | "<![CDATA[" as s { XML_CDATA (s ^ xml_cdata lexbuf) }
  | eof { EOF }
  | '{' { XML_EXP (Parser.exp_rparen token lexbuf)}
  | "" { XML_STR (xml_str lexbuf) }

and xml_attributes = parse
  | space+ { xml_attributes lexbuf }
  | '>' { [], false }
  | "/>" { [], true }
  | '{'
    {
      let a = Parser.exp_rparen token lexbuf in
      let s = xml_eq_value lexbuf in
      let l,r = xml_attributes lexbuf in
      ((a,s)::l, r)
    }
  | (identchar+ as a)
    {
      let s = xml_eq_value lexbuf in
      let l,r = xml_attributes lexbuf in
      ((Ast.EStr a,s)::l, r)
    }

and xml_eq_value = parse
  | space* '=' space* '"' (([^ '"' '\\'] | '\\' ['"' '\''])* as s) '"'
    {
      Ast.EStr s
    }
  | space* '=' space* '{'
    {
      Parser.exp_rparen token lexbuf
    }

and xml_spaces = parse
  | space* { () }

and xml_comment = parse
  | "-->" as s { s }
  | _ as s { (String.make 1 s) ^ (xml_comment lexbuf) }
  | eof { assert false }

and xml_cdata = parse
  | "]]>" as s { s }
  | _ as s { (String.make 1 s) ^ (xml_cdata lexbuf) }
  | eof { assert false }

and xml_str = parse
  | "&amp;" { "&" ^ xml_str lexbuf }
  | "&lt;"  { "<" ^ xml_str lexbuf }
  | "&gt;"  { ">" ^ xml_str lexbuf }
  | "&apos;" { "'" ^ xml_str lexbuf } 
  | "&quot;" { "\"" ^ xml_str lexbuf  }
  | "&#" ['0'-'9']+ ";" as s { s ^ xml_str lexbuf }
  | "&#" ['X' 'x'] ['0'-'9' 'a'-'f' 'A'-'F']+ ";" as s { s ^ xml_str lexbuf }
  | [^ '<' ] as s { (String.make 1 s) ^ xml_str lexbuf }
  | "" { "" }
