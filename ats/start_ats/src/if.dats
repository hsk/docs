// if.dats

#include "share/atspre_staload.hats"

fn f(v:int):int = 
	if v = 10 then 1 else 2

implement main0() = {
	val v:int = 10

	val v2:int = f(v)
	val () = println!("v2=",v2)
}
