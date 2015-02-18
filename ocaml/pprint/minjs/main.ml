open E

(*
問題のある例
*)
module PP = struct
  let nest = ref 0

  let rec ln = function
    | "@["::os | "@]"::os | "@"::os -> ln os
    | "@\n" :: os -> true
    | os -> false

  let rec to_s a sp ts os =
    match(ts,os) with
    | ("@"::t::ts,os) -> to_s (a^t) sp ts os
    | ("@\n"::ts,os) when (ln os) -> to_s a sp ts os
    | (")" as t::ts,os) when !nest > 0 -> nest := !nest - 1; to_s (a^t) sp ts os
    | (ts,"@\n"::os) -> to_s (a ^ "\n" ^ sp) sp ts os
    | (ts,"@["::os) -> to_s a (sp ^ "  ") ts os
    | (ts,"@]"::os) ->
      to_s a
        (String.sub sp 0 ((String.length sp) - 2))
        ts os
    | ("@\n"::ts,os) -> to_s (a^"\n"^ sp) sp ts os
    | (t::ts,o::os) when t = o -> to_s (a^t) sp ts os
    | ("(" as t::ts,os) -> nest := !nest + 1; to_s (a^t) sp ts os
    | (t::ts,os) -> to_s (a^t) sp ts os
    | ([],o::os) -> to_s (a^o) sp ts os
    | ([],_) -> a

  let buf = ref []

  let put (s:string) =
    buf := s::!buf

  let puts (ss) =
    List.iter put ss

  let get ():string =
    let ls = List.rev !buf in
    buf := [];
    to_s "" "" (Lexer.get()) ls 

end

let rec pp = function
  | Var i -> PP.put i
  | App(t1,ts) ->
    pp t1; PP.puts["@[";"{"];
    List.iter begin fun t2 ->
      PP.put "@\n"; pp t2;
    end ts;
    PP.puts["@]";"@\n";"}"]
  | Bin(t1,op,t2) ->
    pp t1; PP.put "+"; pp t2

let lexbuf l =
  pp (Parser.exp Lexer.token l);
  Printf.printf "%s\n" (PP.get())

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
