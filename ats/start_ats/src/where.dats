// where.dats

#include "share/atspre_staload.hats"

fn f():int = b where {
  val a = 1
  val b = 2
  val () = println!("kore")
  val () = println!("kore2")
  val c = a + b
  val d = c
}

implement main0() = {
  val v = f()
  val () = println!("v=",v)
}
