{
open Parser

let str = ref ""
let tokens = ref []

let add s = 
	tokens := s :: !tokens
let t f l =
	let s = f l in
	add (s);
	s
let tt f l r =
	let _ = t f l in
	r

let get () =
	let ts = List.rev !tokens in
	tokens := [];
	ts
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']



rule token = parse
| '\n' space* "/*" {
	add "@\n";
	str := "/*";
	(comment lexbuf);
	token lexbuf
}
| '\n' space* "//" {
	add "@\n";
	str := "//";
	(comment2 lexbuf);
	token lexbuf
}
| "//" {
	add "@";
	str := " //";
	(comment2 lexbuf);
	token lexbuf
}
| "/*" {
	add "@";
	str := " /*";
	(comment lexbuf);
	token lexbuf
}
| '{' { tt Lexing.lexeme lexbuf LBRACE }
| '}' { tt Lexing.lexeme lexbuf RBRACE }
| '(' { tt Lexing.lexeme lexbuf LPAREN }
| ')' { tt Lexing.lexeme lexbuf RPAREN }
| space+ { token lexbuf }
| digit+ { IDENT(t Lexing.lexeme lexbuf) }
| '+' { tt Lexing.lexeme lexbuf ADD }
| [':' '+' '-' '*' '%' '&' '!' '|' '>' '<'] [':' '+' '-' '%' '&' '!' '|' '>' '<']* { IDENT(t Lexing.lexeme lexbuf) }

| eof { EOF }
| (digit|lower|upper|'_')* { IDENT(t Lexing.lexeme lexbuf) }
| _ {
  failwith (Printf.sprintf "unknown token %s near characters %d-%d"
    (Lexing.lexeme lexbuf)
    (Lexing.lexeme_start lexbuf)
    (Lexing.lexeme_end lexbuf))
}

and comment = parse
| "*/" { str := !str ^ "*/"; add !str; ln lexbuf }
| "/*" { str := !str ^ "/*"; comment lexbuf; comment lexbuf }
| eof { str := !str ^ "*/"; Format.eprintf "warning: unterminated comment@." }
| _ { str := !str ^ (Lexing.lexeme lexbuf); comment lexbuf }

and ln = parse
| [' ' '\t']* ['\r' '\n'] { add "@\n"; ()}
| [' ' '\t']* { () }

and comment2 = parse
| ['\r' '\n'] { add !str; add "@\n"; () }
| eof { Format.eprintf "warning: unterminated comment@."; add !str; add "@\n"; () }
| _ { str := !str ^ (Lexing.lexeme lexbuf); comment2 lexbuf }
