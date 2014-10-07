open Syntax

type e =
  | EInt of int
  | EBin of e * string * e
  | EPre of string * e
  | ECall of e * e list
  | ECallM of string * e * e list
  | EArr of e * e list
  | EVar of string
  | EString of string
  | EEmpty
  | ECast of Ty.t * e

let rec print sp e =
  match e with
  | EInt i -> Printf.sprintf "%s%d" sp i
  | EVar i -> Printf.sprintf "%s%s" sp i
  | EString i -> Printf.sprintf "%s%s" sp i
  | EBin(e1,op,e2) -> Printf.sprintf "%s(%s %s %s)" sp (print "" e1) op (print "" e2)
  | EPre(op,e1) -> Printf.sprintf "%s(%s %s)" sp op (print "" e1)
  | ECall(e1,es) -> Printf.sprintf "%s(%s)" (print sp e1) (print_ls (print "") es ~sep:", ")
  | ECallM(i,e1,es) -> Printf.sprintf "%s(%s)" (print sp e1) (print_ls (print "") es ~sep:", ")
  | EArr(e1,es) -> Printf.sprintf "%s[%s]" (print sp e1) (print_ls (print "") es ~sep:", ")
  | ECast(t,e) -> Printf.sprintf "((%s)%s)" (Ty.print sp t) (print "" e)
  | EEmpty -> ""
