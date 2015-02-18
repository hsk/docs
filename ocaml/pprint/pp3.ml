type e =
  | Int of int
  | Let of string * e * e
  | Var of string
  | FCom of string * e
  | RCom of e * string
  | UCom of (string list)* e

let chop str = String.sub str 0 (String.length str - 1)
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
  | FCom(fe,e) ->
    Printf.sprintf "%s%s\n%s" sp fe (pp sp e)
  | RCom(e,fe) ->
    Printf.sprintf "%s%s\n" (chop (pp sp e)) fe
  | UCom([fx;bx;],Let(x,e1,e2)) ->
    Printf.sprintf "%slet%s%s%s=\n%s%sin\n%s"
      sp
      fx x bx
      (pp (sp ^ "  ") e1)
      sp
      (pp sp e2)
  | UCom(ls,e) -> 
    Printf.sprintf "%s%s" (String.concat "" ls) (pp sp e)

let _ =
  let e =
    Let("a",
      Let("b",
        FCom("(*fint*)",Int 1),
      RCom(Var "b","(*bvar*)")),

      RCom(
      FCom("(*Flet*)",
      UCom(["(*Fc*)";"(*Bc*)"],
      Let("c",
        RCom(FCom("(*f1*)",Int 1),"(*r1*)"),
        RCom(FCom("(*fb*)",Var "b"),"(*rb*)")))),"(*Rlet*)"))
  in
  let s = pp "" e in
  Printf.printf "%s\n" s
