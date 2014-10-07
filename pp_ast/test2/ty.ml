open Syntax

  type t =
  | Ty of string
  | TFun of t * t list
  | TPtr of t

  let rec print ?(pp="") sp t =
    match t with
    | Ty(s) -> sp ^ s
    | TPtr(t) -> print ~pp:pp sp t ^ "*"
    | TFun(r,ts) -> Printf.sprintf "%s(*%s)(%s)" (print sp r) pp (print_ls (print "") ts ~sep:", ")

