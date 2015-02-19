open Format

type t =
  | Var of string
  | Let of string * t * t
  | Bin of t * string * t
  | Pre of string * t
  | Post of t * string
  | App of t * t list

let infixs =
  [
    "=",  (1, false);
    "==", (2, true);
    "!=", (2, true);
    "<",  (3, true);
    ">",  (3, true);
    "<=", (4, true);
    ">=", (5, true);
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

let rec pp_ls ppf sep print_fun = function
  | [] -> ()
  | [x] -> print_fun ppf x
  | x :: xs ->
    print_fun ppf x;
    fprintf ppf "%s" sep;
    pp_ls ppf sep print_fun xs

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

  | App(e1, es) ->
    pp true 0 ppf e1;
    fprintf ppf " ";
    pp_ls ppf " " (pp true 0) es

let _ =
  let a = Let("test",Var "a",Var "b")in
  let b = Bin(Var "a","*",Var "b") in
  let c = Bin(b,"*",b) in
  let c = Bin(c,"*",App(Var "a", [Var "c"; Var "d"])) in
  let a = Let("test",a,c) in
  printf "%a\n" (pp true 0 ) a
