type t =
  | Var of string
  | App of t * t list
  | Bin of t * string * t
