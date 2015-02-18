{
open Parser
let comments = ref []
let str = ref ""
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']


rule token = parse
| space* "(*" {
	let start = (Lexing.lexeme_start lexbuf) in
	str := (Lexing.lexeme lexbuf);
	(comment lexbuf);

	comments := (start,!str) :: !comments;
	token lexbuf

}
| space+ { token lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| digit+ { INT(int_of_string (Lexing.lexeme lexbuf)) }
| '=' { push(); EQ }
| "let" { push(); LET }
| "in" { push(); IN }
| eof { EOF }
| lower (digit|lower|upper|'_')* { IDENT(Lexing.lexeme lexbuf) }
| _ {
  failwith (Printf.sprintf "unknown token %s near characters %d-%d"
    (Lexing.lexeme lexbuf)
    (Lexing.lexeme_start lexbuf)
    (Lexing.lexeme_end lexbuf))
}

and comment = parse
| "*)" { str := !str ^ "*)"; ln lexbuf }
| "(*" { str := !str ^ "(*"; comment lexbuf; comment lexbuf }
| eof { str := !str ^ "*)"; Format.eprintf "warning: unterminated comment@." }
| _ { str := !str ^ (Lexing.lexeme lexbuf); comment lexbuf }

and ln = parse
| [' ' '\t']* ['\r' '\n'] { str := !str ^ "\n"; ()}
| [' ' '\t']* { () }