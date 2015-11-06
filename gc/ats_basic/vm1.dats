// patscc -DATS_MEMALLOC_LIBC -L$PATSHOME/ccomp/atslib/lib -latslib test.dats -o test; ./test
#include "share/atspre_staload.hats"
staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/DATS/list0.dats"
#define :: cons0

val () = {
  fun printls(xs: list0(int)) : void =
    begin case+ xs of
      | x :: xs =>
        begin
          println! x;
          printls xs
        end
      | _ => ()
    end

  val () = printls(nil0())
  val () = printls(cons0(1,nil0()))
  val () = printls(list0_of_list($list(0, 2, 4, 6, 8)))
  val () = printls(1::2::3::nil0())
}

val () = {
  datatype e =
    | EInt of (int)
    | EAdd of (e, e)
    | EMul of (e, e)

  fun eval(e:e):int =
    case e of
    | EInt(a) => a
    | EAdd(a,b) => eval(a) + eval(b)
    | EMul(a,b) => eval(a) * eval(b)
  val () = println!(eval(EMul(EAdd(EInt(1),EInt(2)),EInt(10))))
}

val () = {
  datatype C =
    | Int of (int)
    | Add of ()
    | Mul of ()

  fun eval(s:list0(int),c:list0(C)):int =
    case (s,c) of
    | (s, Int(a)::c) => eval(a::s, c)
    | (a::b::s, Add()::c) => eval(a+b::s,c)
    | (a::b::s, Mul()::c) => eval(a*b::s,c)
    | (a::s, _) => a
    | (_,_) => (~1)

  val () = println!(eval(nil0(), Int(1)::Int(2)::Add()::Int(3)::Mul()::nil0()))
}

implement main0 () = {}

