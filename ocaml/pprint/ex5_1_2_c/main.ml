open E
let chop str = String.sub str 0 (String.length str - 1)
let rm src =
  let src = Str.global_replace (Str.regexp "[ \t\r\n]+") " " src in
  let src = Str.global_replace (Str.regexp "[ \t\r\n]*$") "" src in
  let src = Str.global_replace (Str.regexp "^[ \t\r\n]*") "" src in
  src

let comloc = ref 0

let rec pp sp = function
  | Int(i) ->
    Printf.sprintf "%s%d\n" sp i
  | Var(x) ->
    Printf.sprintf "%s%s\n" sp x
  | Let(x,e1,e2) ->
    let x = rm (pp_t (sp ^ "  ") x) in
    let e1 = pp_t (sp ^ "  ") e1 in
    let e2 = pp_t sp e2 in
    Printf.sprintf "%slet %s =\n%s%sin\n%s"
      sp
      x
      e1
      sp
      e2
  | Pos(t) -> pp_t sp t
  | FCom(fe,e) ->
    Printf.sprintf "%s%s\n%s" sp fe (pp sp e)
  | RCom(e,fe) ->
    Printf.sprintf "%s%s\n" (chop (pp sp e)) fe
  | UCom([beq;fin;bin],Let(x,e1,e2)) ->
    Printf.sprintf "%slet %s=%s\n%s%s%sin%s\n%s"
      sp
      (rm (pp_t (sp ^ "  ") x))
      beq
      (pp_t (sp ^ "  ") e1)
      sp fin bin
      (pp_t sp e2)
  | UCom(ls,e) -> 
    Printf.sprintf "%s%s" (String.concat "" ls) (pp sp e)
and pp_t sp = function
|{txt=Pos(t);loc=loc} ->
  let str = (pp_t sp t) in
  Printf.printf "loc=%d\n" loc;
  let com = if (!comloc > loc) then ""
  else (
    let (l,r) = List.partition(fun (k,_) ->
      k <= loc
    ) !Lexer.comments
    in
    let rc = String.concat "" (List.rev(List.map (fun (s,v)->v) l)) in
    Lexer.comments := r;
    rc
  )
  in
  str ^ com
| {txt=txt;loc=loc} ->
  Printf.printf "loc=%d\n" loc;
  let com = if (!comloc > loc) then ""
  else (
    let (l,r) = List.partition(fun (k,_) ->
      k <= loc
    ) !Lexer.comments
    in
    let rc = String.concat "" (List.rev(List.map (fun (s,v)->v) l)) in
    Lexer.comments := r;
    rc
  )
  in
  let str = (pp sp txt) in
  com ^ str

let lexbuf l =
  comloc:=0;
  Parser.lexbuf := l;
  Printf.printf "%s\n" (pp_t "" (Parser.exp Lexer.token l))

let string s = lexbuf (Lexing.from_string s)

let file f =
  let inchan = open_in (f ^ ".ml") in
  try
    lexbuf (Lexing.from_channel inchan);
    close_in inchan;
  with e -> (close_in inchan; raise e)

let () =
  let files = ref [] in
  Arg.parse
    []
    (fun s -> files := !files @ [s])
    ("Min-Caml-JS Compiler\n" ^
     Printf.sprintf "usage: %s ...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
