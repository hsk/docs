type token =
  | EOF
  | INT of (int)
  | LIDENT of (string)
  | LPAREN
  | MINUS
  | OPEN
  | PLUS
  | RPAREN
  | SEMISEMI
  | STAR
  | STRING of (string * string option)
  | UIDENT of (string)
  | UNDERSCORE
  | COMMENT of (string * Location.t)
  | EOL

val implementation :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Parsetree.structure
val interface :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Parsetree.signature
val toplevel_phrase :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Parsetree.toplevel_phrase
val use_file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Parsetree.toplevel_phrase list
val parse_core_type :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Parsetree.core_type
val parse_expression :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Parsetree.expression
val parse_pattern :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Parsetree.pattern
