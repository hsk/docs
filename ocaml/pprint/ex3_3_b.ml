open Format

type t =
  | Var of string
  | Bin of t * string * t
  | Pre of string * t
  | Post of t * string
  | Fun of string * string list * t list
  | App of string * t list

type prog = t list

let infixs =
  [
    "->", (1, false);
    "=",  (5, false);
    "+",  (6, true);
    "-",  (6, true);
    "/",  (7, true);
    "*",  (7, true);
  ]

let prefixs =
  [
    "return", 1;
    "!",      8;
    "-",      8;
  ]

let postfixs =
  [
    "++", 9;
    "--", 9;
  ]

let paren_bin op p =
  let (opp, l) = List.assoc op infixs in
  let (sp, ep) = if p > opp then ("(",")") else ("","") in
  let (p1, p2) = if l then (opp, opp + 1) else (opp + 1, opp) in
  (sp, ep, p1, p2)

let paren_pre op p =
  let opp = List.assoc op prefixs in
  let (sp, ep) = if p > opp then ("(",")") else ("","") in
  (sp, ep, opp + 1)

let paren_post op p =
  let opp = (List.assoc op postfixs) in
  let (sp, ep) = if p > opp then ("(",")") else ("","") in
  (sp, ep, opp)

let sep = function
  | Fun _ -> ""
  | _ -> ";"

let rec pp p ppf t = 
  match t with
  | Var i -> fprintf ppf "%s" i
  | Bin(e1, op, e2) ->
    let (sp, ep, p1, p2) = paren_bin op p in
    fprintf ppf "%s%a %s %a%s"
      sp
      (pp p1) e1
      op
      (pp p2) e2
      ep
  | Pre(op, e1) ->
    let (sp, ep, p1) = paren_pre op p in
    fprintf ppf "%s%s %a%s" sp op (pp p1) e1 ep
  | Post(e1, op) ->
    let (sp, ep, p1) = paren_post op p in
    fprintf ppf "%s%a %s%s" sp (pp p1) e1 op ep
  | App(x,es) ->
    fprintf ppf "%s(%a)" x pps2 es
  | Fun(x, xs, es) ->
    fprintf ppf "@[<2>function %s(%a) {@\n%a@]@\n}"
      x pp_ss xs pps es
and pps ppf = function
  | [] -> ()
  | [e] -> fprintf ppf "%a%s" (pp 0) e (sep e)
  | e::es ->
    fprintf ppf "%a%s@\n%a"
      (pp 0) e
      (sep e)
      pps es
and pps2 ppf = function
  | [] -> ()
  | [e] -> fprintf ppf "%a" (pp 0) e
  | e::es ->
    fprintf ppf "%a, %a"
      (pp 0) e
      pps2 es
and pp_ss ppf = function
  | [] -> ()
  | [s] -> fprintf ppf "%s" s
  | s::ss ->
    fprintf ppf "%s, %a" s pp_ss ss
let _ =
  let prog = [
    Fun("add",["a";"b"],[
      Pre("return",Bin(Var "a","*",Var "b"));
    ]);
    Fun("main",[],[
      App("add",[Var "1";Var "2"]);
      Pre("return", Var "0");
    ]);
    Fun("f",["a";"b";"c";"d"],[
      Pre("return",Bin(Bin(Bin(Var "a","+",Var "b"),"*",Var "c"),"*",Var "d"));
    ]);
    Fun("f",["a";"b";"c";"d"],[
      Pre("return",Bin(Var "a","=",Bin(Var "c","=",Bin(Bin(Bin(Var "a","=",Var "b"),"+",Var "c"),"*",Var "d"))));
    ]);
    Fun("f",["a";"b";"c";"d"],[
      Pre("return",Bin(Var "a","=",Bin(Var "c","+",Bin(Bin(Bin(Var "a","=",Var "b"),"+",Var "c"),"+",Var "d"))));
    ]);
    Bin(Bin(Var "a","->",Var "b"),"->",Bin(Bin(Var "a","->",Var "b"),"->",Bin(Var "a","->",Bin(Var "a","->",Var "b"))));
    Bin(Var "moji","+",Bin(Bin(Var "5","*",Var "2"),"+",Var "3"));
    Bin(Bin(Var "moji","+",Bin(Var "5","*",Var "2")),"+",Var "3");
    Bin(Bin(Var "moji","+",Var "5"),"+",Var "3");
    Bin(Var "a","+",Bin(Var "a","+",Var "b"));

  ] in
  printf "%a\n" pps prog

