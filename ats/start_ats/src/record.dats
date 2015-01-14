// record.dats

#include "share/atspre_staload.hats"

typedef point2D = @{ x= double, y= double }

fn f1(a:point2D):double = begin
  a.x + a.y
end

fn f2(a:point2D):double = begin
  begin case a of
    | @{x=a,y=b} => a + b
  end
end

implement main0() = {
  val e = @{x=1.1,y=2.2}
  val () = println!(e.x)
  val () = println!(f1(e))
}
