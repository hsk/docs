{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']

rule token = parse
| space+
    { token lexbuf }
| "open"
    {  (token2 lexbuf) }
| "/*"
    { comment lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| '[' { LBRACK }
| ']' { RBRACK }
| "return"
    { RETURN}
| "new"
    { NEW }
| "this" { THIS }
| "class" { STRUCT }
| "trait" { TRAIT }
| "<:" { IMPLEMENT }
| ":>" { RIMPLEMENT }
| "=>" { ARROW }
| "->" { MEMBER }
| "|>" { FARROW }
| "if" { IF }
| "else" {ELSE}
| '@' {CASE}
| "::" { ADDLIST }
| "case" {CASE}
| "match" {MATCH}
| "type" {TYPE}
| digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| '-'
    { SUB }
| '+'
    { ADD }
| '*'
    { MUL }
| '&'
    { AMP }
| '<' { LT }
| '>' { GT }
| "<=" { LE }
| ">=" { GE }
| '|' { OR }
| '.' { DOT }
| ','
    { COMMA }
| '^' { HAT }
| ';'
    { SEMICOLON }
| ':'
    { COLON }
| ":=" { COLONASSIGN }
| "#=" { REFASSIGN }
| "def" { DEF }
| "==" { EQ}
| '=' { ASSIGN }
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
| ['a'-'z' 'A'-'Z' '_' '.']* 
    { OPEN(Lexing.lexeme lexbuf) }
| _
    { failwith
      (Printf.sprintf "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)) }

and comment = parse
| "*/"
    { token lexbuf }
| eof
    { failwith
      (Printf.sprintf "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)) }

| _
    { comment lexbuf }
