// patscc -DATS_MEMALLOC_LIBC calc_ref.dats -o calc_ref

#include "share/atspre_staload.hats"

staload     "libats/SATS/refcount.sats"
staload _ = "libats/DATS/refcount.dats"

datavtype E =
  | EInt of int
  | EAdd of (Exp, Exp)
  | ESub of (Exp, Exp)
where Exp = refcnt(E)

fn eint(x: int): Exp = refcnt_make(EInt(x))
fn eadd(e1: Exp, e2: Exp) = refcnt_make(EAdd(e1, e2))
fn esub(e1: Exp, e2: Exp) = refcnt_make(ESub(e1, e2))

fn Exp_copy(e0: !Exp): Exp = (
  refcnt_incref(e0)
)

fun Exp_free(e: Exp): void = {
  var e2: Exp?
  val () = if refcnt_decref(e, e2) then {
    prval() = opt_unsome(e2)
    val () =
      begin case+ e2 of
        | ~EInt(x) => ()
        | ~EAdd(e1, e2) => (Exp_free(e1); Exp_free(e2))
        | ~ESub(e1, e2) => (Exp_free(e1); Exp_free(e2))
      end
  } else {
    prval() = opt_unnone(e2)
  }
}

extern fun Exp_print(!Exp): void
overload print with Exp_print
implement Exp_print(e) = {
  val (pf, fpf | p) = refcnt_vtakeout(e)
  val () =
    begin case+ !p of
      | EInt(x) => print!("EInt(", x, ")")
      | EAdd(e1, e2) => print!("EAdd(", e1, ", ", e2, ")")
      | ESub(e1, e2) => print!("ESub(", e1, ", ", e2, ")")
    end
  prval () = fpf(pf)
}

fun Exp_eval(e: !Exp):int = res where {
  val (pf, fpf | p) = refcnt_vtakeout(e)
  val res =
    begin case+ !p of
      | EInt(x) => x
      | EAdd(e1, e2) => Exp_eval(e1) + Exp_eval(e2)
      | ESub(e1, e2) => Exp_eval(e1) - Exp_eval(e2)
    end
  prval() = fpf(pf)
}

implement main0() = {
  val e = eadd(eint 1, eint 2)
  val e2 = esub(e, Exp_copy e)
  val ans = Exp_eval e2
  val() = println!("eval(", e2, ")")
  val() = Exp_free e2
  val() = println!ans
}

