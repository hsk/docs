open E
let chop str = String.sub str 0 (String.length str - 1)
let inln str sp =
  (String.sub str 0 (String.length str - 4)) ^ "\n"^sp^"in"
let rm src =
  let src = Str.global_replace (Str.regexp "[ \t\r\n]+") " " src in
  let src = Str.global_replace (Str.regexp "[ \t\r\n]*$") "" src in
  let src = Str.global_replace (Str.regexp "^[ \t\r\n]*") "" src in
  src
let cut sp str =
  let ln = (String.length str - String.length sp) in
  if ln > 0 then
    String.sub str (String.length sp) ln
  else
    str
let rec isblock = function
  | FCom(_, e)::xs -> isblock [e] || isblock xs
  | Let(_,_,_)::xs -> true
  | RCom(_)::xs -> isblock xs
  | Cons(a,b)::xs -> isblock [a] || isblock [b] || isblock xs
  | Match(_,_)::xs -> true
  | Tok(_)::xs -> isblock xs
  | Paren(_,e,_)::xs -> isblock[e] || isblock xs
  | [] -> false

let rec pp sp ln = function
  | Tok(x) -> Printf.sprintf "%s " x
  | Cons(a,Tok"end") ->
    Printf.sprintf "%s\n%s%s" (pp sp "" a) (cut "  " sp) "end"
  | Cons(a,b) when isblock [b] ->
    let sp2 = if(a=Tok"(" || a=Tok"begin") then sp ^ "  " else sp in
    Printf.sprintf "%s%s%s" sp (pp sp "" a) (cut sp2 (pp sp2 "" b))

  | Cons(a,b) ->
    let sp = if(a=Tok"begin") then sp ^ "  " else sp in
    Printf.sprintf "%s%s" (pp sp "" a) (pp sp "" b)
  | Paren(l,e1,r) ->
    let e1 = pp sp "" e1 in
    Printf.sprintf "%s%s%s"
      l
      e1
      r

  | Let(e1,e2,e3) when isblock e2 ->
    let e1 = pps sp "" e1 in
    let add3 = not (isblock e3) in
    let e2 = blocks (sp ^ "  ") "\n" e2 in
    let e3 = blocks (sp) "" e3 in
    Printf.sprintf "%s%s\n%s\n%s%s"
      sp
      e1
      (inln e2 sp)
      (if add3 then sp else "")
      e3    
  | Let(e1,e2,e3) ->
    let e1 = pps sp "" e1 in
    let add3 = not (isblock e3) in
    let e2 = pps (sp ^ "  ") "\n" e2 in
    let e3 = pps (sp) "" e3 in
    Printf.sprintf "%s%s%s\n%s%s"
      sp
      e1
      e2
      (if add3 then sp else "")
      e3


and pps sp ln es =
  (String.concat "" (List.map (pp (sp) ln) es))
and blocks sp ln es =
  (String.concat sp (List.map (pp (sp) ln) es))

let lexbuf l =
  Printf.printf "%s\n" (pp "" "\n" (Parser.exp Lexer.token l))

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
