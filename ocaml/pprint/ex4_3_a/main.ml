open E
let chop str = String.sub str 0 (String.length str - 1)
let chop2 str = String.sub str 0 (String.length str - 4)
let rm src =
  let src = Str.global_replace (Str.regexp "[ \t\r\n]+") " " src in
  let src = Str.global_replace (Str.regexp "[ \t\r\n]*$") "" src in
  let src = Str.global_replace (Str.regexp "^[ \t\r\n]*") "" src in
  src

let rec addln = function
  | FCom(_, e)::xs -> addln [e] || addln xs
  | Let(_,_,_)::xs -> true
  | RCom(_)::xs -> addln xs
  | Cons(a,b)::xs -> addln [a] || addln [b]
  | Match(_,_)::xs -> true
  | Tok(_)::xs -> addln xs
  | [] -> false

let rec pp sp ln = function
  | FCom(x, e) ->
    Printf.sprintf "\n%s%s\n%s" sp x (pp sp ln e)
  | RCom(x) ->
    Printf.sprintf "%s" x  
  | Tok(x) ->
    Printf.sprintf "%s " x
  | Let(e1,e2,e3) ->
    let e1 = pps sp "" e1 in
    let e2_ = pps (sp ^ "  ") "\n" e2 in
    let add = addln e2 in
    let e2 = if add then "\n" ^ e2_ else e2_ in
    let e3 = if addln e3 then pps2 (sp) "" e3  else (sp ^ (pps sp "" e3)) in
    Printf.sprintf "%s%s%s%s"
      sp
      e1
      ((chop2 e2) ^ (if add then "\n"^sp^"in\n" else "in\n" ))
      e3

  | Match(e1,es2) ->
    let e1 = pps sp "" e1 in
    let e2 = String.concat (sp ^ "  ") (List.map(fun e2 ->
      pps sp "\n" e2
    ) es2) in
    Printf.sprintf "%s%s\n%s  %s"
      sp
      e1
      sp
      (chop e2)
  | Cons(e1,e2) -> (pp sp ln e1) ^ (pp sp ln e2)

and pps sp ln es =
  (String.concat "" (List.map (pp sp "") es)) ^ ln
and pps2 sp ln es =
  (String.concat sp (List.map (pp sp ln) es)) ^ ln

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
