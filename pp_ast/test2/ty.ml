open Syntax

  type t =
  | Ty of string
  | TFun of t * t list

  let rec print ?(pp="") sp t =
    match t with
    | Ty(s) -> sp ^ s
    | TFun(r,ts) -> Printf.sprintf "%s(*%s)(%s)" (print sp r) pp (print_ls (print "") ts ~sep:", ")

