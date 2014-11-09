{
open Parser
let heads = ref []
}

let space = [' ' '\t']
let digit = ['0'-'9']

rule token = parse
  | '\r' '\n' { incr lineno; token lexbuf }
  | ['\n' '\r'] { incr lineno; token lexbuf }
  | space+ { token lexbuf }
  | "/*" {
      comment lexbuf;
      token lexbuf
    }
  | "%{" {
      head lexbuf;
      STARTS (head2 lexbuf);
  }
  | "%%" { EOF }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | digit+ { INT(int_of_string (Lexing.lexeme lexbuf)) }
  | '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A' - 'F']+ { INT(int_of_string (Lexing.lexeme lexbuf)) }
  | '0' ['o' 'O'] ['0'-'7']+ { INT(int_of_string (Lexing.lexeme lexbuf)) }
  | '0' ['b' 'B'] ['0'-'1']+ { INT(int_of_string (Lexing.lexeme lexbuf)) }
  | digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(Lexing.lexeme lexbuf) }  

  | '|' { OR }
  | ':' { COLON }
  | ';' { SEMI }
  | '"' ([^ '"'] | "\\" (['"'  '\'' 'n' 'r' 't' 'b' ] | ['0'-'9'] ['0'-'9'] ['0'-'9']) )* '"' { STR(Lexing.lexeme lexbuf) }
  | '\'' ([^ '\''] | "\\" (['"'  '\'' 'n' 'r' 't' 'b' ] | ['0'-'9'] ['0'-'9'] ['0'-'9']) )* '\'' { CHR(Lexing.lexeme lexbuf) }

  | ['`' 'a'-'z' 'A'-'Z' '_' '$' ',' '=' '+' '-' '*' '/' '%' '>' '<'
    '?' '&' '@'
    '.' '~' '^']
    [    'a'-'z' 'A'-'Z' '_' '$' ',' '0'-'9' ]*
      { VAR(Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _
    {
      failwith (
        parse_error2 (
          Printf.sprintf "unknown token %s line %d"
            (Lexing.lexeme lexbuf)
            !lineno
        )
      )
    }

and comment = parse
  | "*/" { () }
  | '\r' '\n' { incr lineno; comment lexbuf }
  | ['\n' '\r'] { incr lineno; comment lexbuf }

  | "/*" { comment lexbuf; comment lexbuf }
  | eof { Format.eprintf "warning: unterminated comment@." }
  | _ { comment lexbuf }
and head = parse
  | "%}" { heads := []; () }
  | '\r' '\n' { incr lineno; head lexbuf }
  | ['\n' '\r'] { incr lineno; head lexbuf }

  | eof { Format.eprintf "warning: unterminated head@." }
  | _ { head lexbuf }
and head2 = parse
  | "%%" { let l = !heads in heads := []; l }
  | "%start" space+ ((['`' 'a'-'z' 'A'-'Z' '_' '$' ',']
    [    'a'-'z' 'A'-'Z' '_' '$' ',' '0'-'9' ]*) as a)
    { heads := a::(!heads); head2 lexbuf }
  | '\r' '\n' { incr lineno; head2 lexbuf }
  | ['\n' '\r'] { incr lineno; head2 lexbuf }
  | _ { head2 lexbuf }
