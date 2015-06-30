datatype e =
  | Int of (int)
  | Bin of (e, string, e)

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/DATS/list0.dats"
staload "libats/ML/SATS/filebas.sats"

fun show_e(e:e):void
fun read_all(filename:string):list0(char)
