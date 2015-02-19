{
open Parser

let str = ref ""
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']


rule token = parse
| '\n' space* "(*" {
	str := "(*";
	(comment lexbuf);
	FCOM(!str)
}
| "(*" {
	str := "(*";
	(comment lexbuf);
	COM(!str)
}
| "()" { IDENT(Lexing.lexeme lexbuf) }
| '{' { LPAREN(Lexing.lexeme lexbuf)}
| '}' { RPAREN(Lexing.lexeme lexbuf)}
| '(' { IDENT(Lexing.lexeme lexbuf) }
| ')' { IDENT(Lexing.lexeme lexbuf) }
| space+ { token lexbuf }
| digit+ { IDENT(Lexing.lexeme lexbuf) }
| '=' { EQ(Lexing.lexeme lexbuf) }
| '|' { BAR("|")}
| [':' '+' '-' '*' '/' '%' '&' '!' '|' '>' '<']+ { IDENT(Lexing.lexeme lexbuf) }
| "let" { LET(Lexing.lexeme lexbuf) }
| "in" { IN(Lexing.lexeme lexbuf) }
| "match" { MATCH(Lexing.lexeme lexbuf)}
| "with" { WITH(Lexing.lexeme lexbuf)}
| eof { EOF }
| (digit|lower|upper|'_')* { IDENT(Lexing.lexeme lexbuf) }
| _ {
  failwith (Printf.sprintf "unknown token %s near characters %d-%d"
    (Lexing.lexeme lexbuf)
    (Lexing.lexeme_start lexbuf)
    (Lexing.lexeme_end lexbuf))
}

and comment = parse
| "*)" { str := !str ^ "*)"; () }
| "(*" { str := !str ^ "(*"; comment lexbuf; comment lexbuf }
| eof { str := !str ^ "*)"; Format.eprintf "warning: unterminated comment@." }
| _ { str := !str ^ (Lexing.lexeme lexbuf); comment lexbuf }

and ln = parse
| [' ' '\t']* ['\r' '\n'] { str := !str ^ "\n"; ()}
| [' ' '\t']* { () }