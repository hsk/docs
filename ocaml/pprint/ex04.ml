open Format

type t =
  | Var of string
  | Let of string * t * t
  | Bin of t * string * t
  | Pre of string * t
  | Post of t * string

let infixs =
  [
    "=",  (1, false);
    "+",  (6, true);
    "-",  (6, true);

    "/",  (7, true);
    "*",  (7, true);
  ]

let prefixs =
  [
    "new", (8, true);
    "!",   (8, false);
    "-",   (8, false);
  ]

let postfixs =
  [
    "++", 9;
    "--", 9;
  ]

let rec pp paren p ppf t = 
  match t with
  | Var i -> fprintf ppf "%s" i
  | Let(s,(Let _ as ts),s2) ->
    fprintf ppf "@[<2>let %s = @\n%a@]@\nin@\n%a"
      s (pp true 0) ts (pp true 0) s2
  | Let(s,ts,s2) ->
    fprintf ppf "let %s = %a in@\n%a" s (pp true 0) ts (pp true 0) s2

  | Pre(op, e1) ->

    let (p1,ident) = (List.assoc op prefixs) in
    let paren = paren && p1 < p in

    if paren then fprintf ppf "(";
    fprintf ppf " %s" op;
    if ident then fprintf ppf " ";
    pp true p1 ppf e1;
    if paren then fprintf ppf ")"

  | Post(e1, op) ->

    let p1 = (List.assoc op postfixs) in
    let paren = paren && p1 <= p in

    if paren then fprintf ppf "(";
    fprintf ppf " %s%a" op (pp true (p1 - 1)) e1;
    if paren then fprintf ppf ")"

  | Bin(e1, op, e2) ->
    let (p1, l) = (List.assoc op infixs) in
    let paren = paren && (if l then p1 <= p else p1 < p) in
    if paren then fprintf ppf "(";
    pp paren (if l then p1 - 1 else p1 + 1) ppf e1;
    fprintf ppf " %s " op;
    pp true p1 ppf e2;
    if paren then fprintf ppf ")"

let _ =
  let a = Let("test",Var "a",Var "b")in
  let b = Bin(Var "a","*",Var "b") in
  let c = Bin(b,"*",b) in
  let c = Bin(c,"*",Var "a") in
  let a = Let("test",a,c) in
  printf "%a\n" (pp true 0 ) a