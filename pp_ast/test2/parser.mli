type token =
  | INT of (int)
  | ID of (string)
  | SUB
  | ADD
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | PRINT
  | EOF
  | COLON
  | COMMA
  | ASSIGN
  | RETURN
  | INCLUDE of (string)
  | STRING of (string)
  | STRUCT
  | THIS
  | DOT
  | IF
  | ELSE
  | IMPLEMENT
  | RIMPLEMENT
  | TRAIT
  | ARROW
  | MEMBER
  | LT
  | GT
  | LE
  | GE
  | MUL
  | AMP
  | DIV
  | CAST
  | NEW
  | AT

val stmts :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Stmt.t list
