open Syntax
open Format

type t =
  | Ty of string
  | TFun of t * t list
  | TPtr of t

let rec print pp sp ppf = function
  | Ty(s) ->
    fprintf ppf "%s%s"
      sp
      s
  | TPtr(t) ->
    fprintf ppf "%a*"
      (print pp sp) t
  | TFun(r,ts) ->
    fprintf ppf "%a(*%s)(%a)"
      (print "" sp) r
      pp
      (print_ls ", " (print "" "")) ts
