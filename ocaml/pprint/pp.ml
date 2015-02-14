type e =
  | Int of int
  | Let of string * e * e
  | Var of string
let rec pp sp = function
  | Int(i) ->
    Printf.sprintf "%s%d\n" sp i
  | Var(x) ->
    Printf.sprintf "%s%s\n" sp x
  | Let(x,e1,e2) ->
    Printf.sprintf "%slet %s =\n%s%sin\n%s"
      sp
      x
      (pp (sp ^ "  ") e1)
      sp
      (pp sp e2)

let _ =
  let e = Let("a",Let("b", Int 1, Var "b"),Let("c", Int 1, Var "b")) in
  let s = pp "" e in
  Printf.printf "%s\n" s