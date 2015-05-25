// string_tail.dats

#include "share/atspre_staload.hats"

implement main0() = {
	val a = "add"
	val () = println!("'", a, "' tail is ", string_tail(a) )
}
