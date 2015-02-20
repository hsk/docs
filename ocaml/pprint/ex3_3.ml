open Format

type t =
  | Var of string
  | Bin of t * string * t

let order_of_bin op p =
  let (opp, l) = match op with
    | "::" -> (5, false)
    | "+"  -> (6,  true)
    | "*"  -> (7,  true)
    | _    -> (10, true)
  in
  let (p1, p2) = if l then (opp, opp + 1) else (opp + 1, opp) in
  let (lparen, rparen) = if p > opp then ("(",")") else ("","") in
  (lparen, rparen, p1, p2)

let rec pp p ppf t = 
  match t with
  | Var i -> fprintf ppf "%s" i
  | Bin(e1, op, e2) ->
    let (lparen, rparen, p1, p2) = order_of_bin op p in
    fprintf ppf "%s%a %s %a%s" lparen (pp p1) e1 op (pp p2) e2 rparen

let _ =
  let prog = [
    Bin(Bin(Var "a", "+" , Var  "b" ), "*",  Bin(Var "c", "+",  Var "d"));
    Bin(Bin(Var "a", "*" , Var  "b" ), "+",  Bin(Var "c", "*",  Var "d"));
    Bin(Bin(Var "a", "+" , Var "'b'"), "+",  Bin(Var "c", "+",  Var "d"));
    Bin(Bin(Var "a", "::", Var  "b" ), "::", Bin(Var "c", "::", Var "d"));
  ] in
  List.iter begin fun e ->
    printf "%a\n" (pp 0) e
  end prog

let _ =
  let test t s =
    fprintf str_formatter "%a" (pp 0) t;
    assert (s = (flush_str_formatter()))
  in
  test (Bin(Bin(Var "a", "+" , Var  "b" ), "*",  Bin(Var "c", "+",  Var "d"))) "(a + b) * (c + d)";
  test (Bin(Bin(Var "a", "*" , Var  "b" ), "+",  Bin(Var "c", "*",  Var "d"))) "a * b + c * d";
  test (Bin(Bin(Var "a", "+" , Var "'b'"), "+",  Bin(Var "c", "+",  Var "d"))) "a + 'b' + (c + d)";
  test (Bin(Bin(Var "a", "::", Var  "b" ), "::", Bin(Var "c", "::", Var "d"))) "(a :: b) :: c :: d";
