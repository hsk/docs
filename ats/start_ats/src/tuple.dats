// tuple.dats

#include "share/atspre_staload.hats"

fn f1(a:(int,int)):int = a.0
fn f2(a:(int,int)):int = let
	val (x,y) = a
in
	x
end

fn f3(a:(int,int)):int = let
	val x = case a of _ => 0
in
	x
end

implement main0() = {
  val e = (1,2)
  val () = println!(e.0)
  val () = println!(e.1)
  val () = println!(f1(e))
  val () = println!(f2(e))
  val () = println!(f3(e))
}
