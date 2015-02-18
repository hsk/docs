type token =
  | INT of (int)
  | IDENT of (string)
  | EQ
  | LET
  | IN
  | LPAREN
  | RPAREN
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open E
let l = Lexing.from_string ""
let lexbuf = ref 
{
  	Lexing.refill_buff = (fun lexbuf -> ());
  	Lexing.lex_buffer = "";
  	Lexing.lex_buffer_len = 0;
  	Lexing.lex_abs_pos = 0;
  	Lexing.lex_start_pos = 0;
  	Lexing.lex_curr_pos = 0;
  	Lexing.lex_last_pos = 0;
  	Lexing.lex_last_action = 0;
  	Lexing.lex_eof_reached = false;
  	Lexing.lex_mem = [||];
  	Lexing.lex_start_p  = Lexing.dummy_pos;
  	Lexing.lex_curr_p = Lexing.dummy_pos;
}
let symloc () = !lexbuf.Lexing.lex_start_pos 

let stack = ref []

let push() = 
	Printf.printf "push %d\n" (symloc());
	stack := symloc() :: !stack

let txt1 e =
	let loc = match !stack with
	| c::xs -> stack := xs; c
	in
	{txt=e;loc=loc}


let txtb t =
	let loc = match !stack with
	| c::xs -> stack := xs; c
	in
	{txt=Pos t;loc=loc}

let txt e =
	{txt=e;loc=symloc()}

# 56 "parser.ml"
let yytransl_const = [|
  259 (* EQ *);
  260 (* LET *);
  261 (* IN *);
  262 (* LPAREN *);
  263 (* RPAREN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\003\000\001\000\001\000\001\000\000\000"

let yylen = "\002\000\
\003\000\001\000\001\000\000\000\001\000\006\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\007\000\002\000\003\000\000\000\000\000\008\000\
\005\000\000\000\000\000\000\000\001\000\000\000\000\000\006\000"

let yydgoto = "\002\000\
\008\000\009\000\000\000"

let yysindex = "\002\000\
\009\255\000\000\000\000\000\000\000\000\000\255\009\255\000\000\
\000\000\001\255\005\255\009\255\000\000\002\255\009\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\249\255\008\000\000\000"

let yytablesize = 15
let yytable = "\011\000\
\004\000\005\000\001\000\012\000\014\000\007\000\015\000\016\000\
\003\000\004\000\005\000\013\000\006\000\010\000\007\000"

let yycheck = "\007\000\
\001\001\002\001\001\000\003\001\012\000\006\001\005\001\015\000\
\000\001\001\001\002\001\007\001\004\001\006\000\006\001"

let yynames_const = "\
  EQ\000\
  LET\000\
  IN\000\
  LPAREN\000\
  RPAREN\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : E.t) in
    Obj.repr(
# 63 "parser.mly"
                    ( _2 )
# 125 "parser.ml"
               : 'simple_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 64 "parser.mly"
      ( txt(Int(_1)) )
# 132 "parser.ml"
               : 'simple_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
        ( txt(Var(_1)) )
# 139 "parser.ml"
               : 'simple_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
   ( symloc() )
# 145 "parser.ml"
               : 'p))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_exp) in
    Obj.repr(
# 69 "parser.mly"
             ( _1 )
# 152 "parser.ml"
               : E.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'simple_exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : E.t) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : E.t) in
    Obj.repr(
# 70 "parser.mly"
                                              ( txt1 (Let(txtb(_2), txtb(_4), _6)) )
# 161 "parser.ml"
               : E.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
    ( failwith
    (Printf.sprintf "parse error near characters %d-%d"
       (Parsing.symbol_start ())
       (Parsing.symbol_end ())) )
# 170 "parser.ml"
               : E.t))
(* Entry exp *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let exp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : E.t)
