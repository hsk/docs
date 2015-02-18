%{
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

%}

%token <int> INT
%token <string> IDENT
%token EQ
%token LET
%token IN
%token LPAREN
%token RPAREN
%token EOF

%right prec_let
%left EQ

%type <E.t> exp
%start exp

%%

simple_exp:
| LPAREN exp RPAREN { $2 }
| INT { txt(Int($1)) }
| IDENT { txt(Var($1)) }
p:
|  { symloc() }
exp:
| simple_exp { $1 }
| LET simple_exp EQ exp IN exp %prec prec_let { txt1 (Let(txtb($2), txtb($4), $6)) }
| error
    { failwith
    (Printf.sprintf "parse error near characters %d-%d"
       (Parsing.symbol_start ())
       (Parsing.symbol_end ())) }

