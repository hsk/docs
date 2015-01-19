// patscc -lgc -DATS_MEMALLOC_GCBDW calc.dats -o calc

#include "share/atspre_staload.hats"

datatype exp =
  | EInt of (int)
  | EAdd of (exp, exp)
  | ESub of (exp, exp)

fun eval(exp:exp):int =
  begin case exp of
    | EInt(i) => i
    | EAdd(a, b) => eval(a) + eval(b)
    | ESub(a, b) => eval(a) + eval(b)
  end

implement main0() = {
  val () = println!(eval(ESub(EAdd(EInt(1),EInt(2)),EInt(1))))
}

