// datatype.dats

#include "share/atspre_staload.hats"

datatype e =
  | ENil of ()
  | EInt of (int)
  | EAdd of (e, e)
  | ESub of (e, e)

fun eval(e:e):int =
  begin case e of
    | ENil()  => 0
    | EInt(i) => i
    | EAdd(a, b) => eval(a) + eval(b)
    | ESub(a, b) => eval(a) + eval(b)
  end

implement main0() = {
  val () = println!(eval(ESub(EAdd(EInt(1),EInt(2)),EInt(1))))
}
