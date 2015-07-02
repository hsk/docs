staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/DATS/list0.dats"
staload "libats/ML/SATS/filebas.sats"

datatype e =
  | Int of (int)
  | Bin of (e, string, e)
  | Var of (string)
  | Let of (string, e, e)

fun show_e(e:e):void
fun read_charlst(filename:string):list0(char)
fun read_all(filename:string):string
