{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']

rule token = parse
| space+
    { token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '{'
    { LBRACE }
| '}'
    { RBRACE }
| "return"
    { RETURN}
| "include"
    {  (token2 lexbuf) }
| "this" { THIS }
| "struct" { STRUCT }
| "trait" { TRAIT }
| "<:" { IMPLEMENT }
| "=>" { ARROW }
| "->" { MEMBER }
| "if" { IF }
| "else" {ELSE}
| digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| '-'
    { SUB }
| '+'
    { ADD }
| '<' { LT }
| '>' { GT }
| "<=" { LE }
| ">=" { GE }
| '.' { DOT }
| ','
    { COMMA }
| ';'
    { SEMICOLON }
| ':'
    { COLON }
| '='
    { ASSIGN }
| '"' [^ '"']* '"' 
    { STRING(Lexing.lexeme lexbuf) }
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
    { ID(Lexing.lexeme lexbuf) }
| eof
    { EOF }
| _
    { failwith
      (Printf.sprintf "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)) }

and token2 = parse
| space+
    { token2 lexbuf }
| eof
    { EOF }
| '"' [^ '"']* '"' 
    { INCLUDE(Lexing.lexeme lexbuf) }
| ['a'-'z' 'A'-'Z' '_' '.']* 
    { INCLUDE("<" ^ Lexing.lexeme lexbuf ^ ">") }
| _
    { failwith
      (Printf.sprintf "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)) }
