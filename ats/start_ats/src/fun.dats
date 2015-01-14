// fun.dats

#include "share/atspre_staload.hats"

fun sum(n:int):int = begin
  if n = 0
  then 0
  else n+sum(n-1)
end

implement main0() = {
  val () = println!("sum(10)=",sum(10))
}
