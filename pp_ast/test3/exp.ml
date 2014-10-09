open Syntax
open Format

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

let rec print sp ppf e =
  match e with
  | EInt i -> fprintf ppf "%s%d" sp i
  | EVar i -> fprintf ppf "%s%s" sp i
  | EString i -> fprintf ppf "%s%s" sp i
  | EBin(e1,op,e2) -> fprintf ppf "%s(%a %s %a)" sp (print "") e1 op (print "") e2
  | EPre(op,e1) -> fprintf ppf "%s(%s %a)" sp op (print "") e1
  | ECall(e1,es) -> fprintf ppf "%a(%a)" (print sp) e1 (print_ls ", " (print "")) es
  | ECallM(i,e1,es) -> fprintf ppf "%a(%a)" (print sp) e1 (print_ls ", " (print "")) es
  | EArr(e1,es) -> fprintf ppf "%a[%a]" (print sp) e1 (print_ls ", " (print "")) es
  | ECast(t,e) -> fprintf ppf "((%a)%a)" (Ty.print "" sp) t (print "") e
  | EEmpty -> ()
