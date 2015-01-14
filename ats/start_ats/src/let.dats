// let.dats

#include "share/atspre_staload.hats"

fn dt():int = let
  val a = 1
  val b = 2
  val c = a + b
  val d = 3
in
  c + d
end

implement main0() = let
  val v = dt()
  val () = println!("v=",v)
  val () = println!("n=",let val n = 1 in n end)
in
  ()
end
