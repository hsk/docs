type e =
| EBin of (e * string * e)
| EInt of int
