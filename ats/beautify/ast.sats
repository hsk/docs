staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/DATS/list0.dats"
staload "libats/ML/SATS/filebas.sats"

datatype t =
  | Ty of (string)
  | TFun of (t, list0(t))
  | TPtr of (t)

datatype e =
  | EInt of (int)
  | EBin of (e, string, e)
  | EPre of (string, e)
  | ECall of (e, list0(e))
  | ECallM of (string, e, list0(e))
  | EArr of (e, list0(e))
  | EVar of (string)
  | EString of (string)
  | EEmpty of ()
  | ECast of (t, e)


datatype s = 
  | SBlock of (list0(s))
  | SIf of (e, s, s)
  | SEmpty of ()
  | SExp of (e)
  | SRet of (e)
  | SFun of (t, string, list0(Id), s)
  | SInclude of (string)
  | SLet of (t, e, e)
  | SStruct of (string, string, list0(TS))
  | SCon of (list0(Id), list0(e), s)
  | STrait of (string, list0(TS))
  | SImpl of (string, string, list0(s))
  | SList of (list0(s))
and TS =
  | TS of (t, s)
and Id =
  | Id of (t, string)

fun show_e(e:e):void
fun read_charlst(filename:string):list0(char)
fun read_all(filename:string):string
