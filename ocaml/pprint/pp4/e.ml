type e =
  | Int of int
  | Let of t * t * t
  | Var of string
  | Pos of t
  | FCom of string * e
  | RCom of e * string
  | UCom of (string list)* e
and t = {txt:e;loc:int}
