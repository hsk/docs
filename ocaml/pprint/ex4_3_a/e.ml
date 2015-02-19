type e =
  | Tok of string
  | Let of e list * e list * e list
  | Match of e list * e list list
  | Cons of e * e
  | FCom of string * e
  | RCom of string
