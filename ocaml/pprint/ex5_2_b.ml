module PP = struct
  let rec ln = function
    | "@["::os | "@]"::os | "@"::os -> ln os
    | "@\n" :: os -> true
    | os -> false

  let rec to_s a sp ts os =
    match(ts,os) with
    | ("@"::t::ts,os) -> to_s (a^t) sp ts os
    | ("@\n"::ts,os) when (ln os) -> to_s a sp ts os
    | (ts,"@\n"::os) -> to_s (a ^ "\n" ^ sp) sp ts os
    | (ts,"@["::os) -> to_s a (sp ^ "  ") ts os
    | (ts,"@]"::os) ->
      to_s a
        (String.sub sp 0 ((String.length sp) - 2))
        ts os
    | ("@\n"::ts,os) -> to_s (a^"\n"^ sp) sp ts os
    | (t::ts,o::os) when t = o -> to_s (a^t) sp ts os
    | (t::ts,os) -> to_s (a^t) sp ts os
    | ([],[]) -> a
    | _ -> assert false
  
  let buf = ref []

  let put (s:string) =
    buf := s::!buf

  let get () =
    let ls = List.rev !buf in
    buf := [];
    ls
end

type t =
  | Var of string
  | App of t * t list

let rec pp = function
  | Var i -> PP.put i
  | App(t1,ts) ->
    pp t1;
    PP.put "@[";
    PP.put "(";
    List.iter begin
      fun t2 ->
        PP.put "@\n";
        pp t2;
    end ts;
    PP.put "@]";
    PP.put "@\n";
    PP.put ")"

let _ =
  let a = [
  "@";"/*b*/";"@\n";"a";"@";"/**/";"(";"@";"/*a*/";"@\n";
    "1";"@";"/**/";"@\n";
  ")";"@";"/*end*/";"(";"2";")"] in
  pp (App(App(Var "a", [Var "1"]),[Var "2"]));

  let b = PP.get() in
  let str = PP.to_s "" "" a b in
  Printf.printf "%s\n" str;
  ()

let _ =
  let a = ["a";"(";"1";")";"(";"2";")"] in
  pp (App(App(Var "a", [Var "1"]),[Var "2"]));

  let b = PP.get() in
  let str = PP.to_s "" "" a b in
  Printf.printf "%s\n" str;
  ()


let _ =
  let a = ["a";"(";"a";"(";"2";")";")"] in
  pp (App(Var "a", [App(Var "a",[Var "2"])]));

  let b = PP.get() in
  let str = PP.to_s "" "" a b in
  Printf.printf "%s\n" str;
  ()

let _ =
  let a = [
    "a";"(";
      "a";"(";
        "2";
        "2";
      ")";
    ")"] in
  pp (App(Var "a", [App(Var "a",[Var "2";Var "2"])]));

  let b = PP.get() in
  let str = PP.to_s "" "" a b in
  Printf.printf "%s\n" str;
  ()

let _ =
  let a = [
    "// aa";"@\n";
    "a";"(";
      "// aa";"@\n";
      "a";"(";
        "2";"@";"// a";"@\n";
        "// a";"@\n";
        "2";"@";"// a";"@\n";
      ")";
      "// aa";"@\n";
      "a";"(";
        "2";"@";"// a";"@\n";
        "2";"@";"// a";"@\n";
      ")";
    ")"] in
  pp (
  App(Var "a", [
    App(Var "a",[Var "2";Var "2"]);
    App(Var "a",[Var "2";Var "2"]);
  ]));

  let b = PP.get() in
  let str = PP.to_s "" "" a b in
  Printf.printf "%s\n" str;
  ()

