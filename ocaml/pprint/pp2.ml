type e =
  | Int of int
  | Let of string * e * e
  | Var of string
  | UCom of (string list) * e
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
  | UCom([fi;bi],Int(i)) ->
    Printf.sprintf "%s%s%d%s\n" sp fi i bi
  | UCom([fx;bx],Var(x)) ->
    Printf.sprintf "%s%s%s%s\n" sp fx x bx
  (* let x = b in c *)
  (* x = in が追加情報が必要　*)
  (* Let of string * e * e *)
  | UCom([flet;fx;feq;beq;fin;bin;blet],Let(x,e1,e2)) ->
    Printf.sprintf "%s%slet%s%s%s=%s\n%s%sin%s\n%s%s%s"
      sp flet
      fx x
      feq beq
      (pp (sp ^ "  ") e1)
      fin bin
      sp (pp sp e2)
      blet
  | UCom(ls,e) -> 
    Printf.sprintf "%s%s" (String.concat "" ls) (pp sp e)

let _ =
  let e =
    Let("a",
      Let("b",
        UCom(["(*fint*)";"(*bint*)"],Int 1),
      UCom(["(*fvar*)";"(*bvar*)"],Var "b")),
      UCom(["(*Flet*)\n";"(*Fx*)";"(*Feq*)";"(*Beq*)";"(*Fin*)";"(*Bin*)";"(*Blet*)"],
      Let("c",
        Int 1,
        Var "b")))
  in
  let s = pp "" e in
  Printf.printf "%s\n" s
