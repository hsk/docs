open Format

type t =
  | Var of string
  | Bin of t * string * t
  | Pre of string * t
  | Post of t * string

let paren_bin op p =
  let (opp, l) = match op with
    | "="  -> (1, false)
    | "::" -> (5, false)
    | "+"  -> (6,  true)
    | "-"  -> (6,  true)
    | "/"  -> (7,  true)
    | "*"  -> (7,  true)
    | _    -> (10, true)
  in
  let (p1, p2) = if l then (opp, opp + 1) else (opp + 1, opp) in
  let (lparen, rparen) = if p > opp then ("(",")") else ("","") in
  (lparen, rparen, p1, p2)

let paren_pre op p =
  let opp = match op with
    | "return" -> 1
    | "!"      -> 8
    | "-"      -> 8
    | _        -> 10
  in
  let (lparen, rparen) = if p > opp then ("(",")") else ("","") in
  (lparen, rparen, opp + 1)

let paren_post op p =
  let opp = match op with
    | "++" -> 9
    | "--" -> 9
    | _    -> 10
  in
  let (lparen, rparen) = if p > opp then ("(",")") else ("","") in
  (lparen, rparen, opp)

let rec pp p ppf t = 
  match t with
  | Var i -> fprintf ppf "%s" i
  | Bin(e1, op, e2) ->
    let (lparen, rparen, p1, p2) = paren_bin op p in
    fprintf ppf "%s%a %s %a%s" lparen (pp p1) e1 op (pp p2) e2 rparen
  | Pre(op, e1) ->
    let (lparen, rparen, p1) = paren_pre op p in
    fprintf ppf "%s%s %a%s" lparen op (pp p1) e1 rparen
  | Post(e1, op) ->
    let (lparen, rparen, p1) = paren_post op p in
    fprintf ppf "%s%a %s%s" lparen (pp p1) e1 op rparen

let _ =
  let prog = [
    Pre("return",Bin(Var "a","*",Var "b"));
    Pre("return", Var "0");
    Pre("return",Bin(Bin(Bin(Var "a","+",Var "b"),"*",Var "c"),"*",Var "d"));
    Pre("return",Bin(Var "a","=",Bin(Var "c","=",Bin(Bin(Bin(Var "a","=",Var "b"),"+",Var "c"),"*",Var "d"))));
    Pre("return",Bin(Var "a","=",Bin(Var "c","+",Bin(Bin(Bin(Var "a","=",Var "b"),"+",Var "c"),"+",Var "d"))));
    Bin(Bin(Var "a","::",Var "as"),"::",Bin(Bin(Var "a","::",Var "as"),"::",Bin(Var "as","::",Bin(Var "as","::",Var "ass"))));
    Bin(Var "moji","+",Bin(Bin(Var "5","*",Var "2"),"+",Var "3"));
    Bin(Bin(Var "moji","+",Bin(Var "5","*",Var "2")),"+",Var "3");
    Bin(Bin(Var "moji","+",Var "5"),"+",Var "3");
    Bin(Var "a","+",Bin(Var "a","+",Var "b"));

  ] in
  List.iter begin fun e ->
    printf "%a\n" (pp 0) e
  end prog
