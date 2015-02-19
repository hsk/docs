let a = ["a";"(";"/*a*/";"1";")"]
let b = ["@[";"a";"(";"@\n";"1";"@]";"@\n";")"]

let rec to_s a sp ts os =
  match(ts,os) with
  | (ts,"@["::os) -> to_s a (sp ^ "  ") ts os
  | (ts,"@\n"::os) -> to_s (a ^ "\n" ^ sp) sp ts os
  | (ts,"@]"::os) ->
    to_s a
      (String.sub sp 0 ((String.length sp) - 2))
      ts os
  | (t::ts,o::os) when t = o -> to_s (a^t) sp ts os
  | (t::ts,os) -> to_s (a^t) sp ts os
  | ([],[]) -> a
  | _ -> assert false

let _ =
  let str = to_s "" "" a b in
  Printf.printf "%s\n" str;
  ()

module PP = struct
  let buf = ref []

  let put (s:string) =
    buf := s::!buf

  let rec list_of_string = function
    | "" -> [] 
    | ch ->
      (String.sub ch 0 1) ::
      (list_of_string(String.sub ch 1 ((String.length ch)-1)))

  let rec parse = function
    | "@"::"["::ss -> "@[" :: parse ss
    | "@"::"]"::ss -> "@]" :: parse ss
    | "@"::"\n"::ss -> "@\n" :: parse ss
    | s::ss -> s :: parse ss

  let p s =
    let ss = list_of_string s in
    let ss = parse ss in
    buf := ss @ !buf
    
  let get () =
    let ls = List.rev !buf in
    buf := [];
    ls
end

let _ =
  PP.put "@[";
  PP.put "a";
  PP.put "(";
  PP.put "@\n";
  PP.put "1";
  PP.put "@]";
  PP.put "@\n";
  PP.put ")";
  let b = PP.get() in
  let str = to_s "" "" a b in
  Printf.printf "%s\n" str;
  ()

type t =
  | Var of string
  | App of t * t

let rec pp = function
  | Var i -> PP.put i
  | App(t1,t2) ->
    PP.put "@[";
    pp t1;
    PP.put "(";
    PP.put "@\n";
    pp t2;
    PP.put "@]";
    PP.put "@\n";
    PP.put ")";
    PP.put "@\n"

let _ =
  let a = ["/*b*/";"a";"/**/";"(";"/*a*/";"1";"/**/";")";"/*end*/"] in
  pp (App(Var "a", Var "1"));

  let b = PP.get() in
  let str = to_s "" "" a b in
  Printf.printf "%s\n" str;
  ()
