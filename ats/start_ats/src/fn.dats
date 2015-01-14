// fn.dats

#include "share/atspre_staload.hats"

fn add(a:int,b:int):int = begin
  a + b
end

implement main0() = {
  val () = println!("add(1,2)=",add(1,2))
}
