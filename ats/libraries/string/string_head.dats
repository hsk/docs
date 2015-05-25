// string_head.dats

#include "share/atspre_staload.hats"

implement main0() = {
	val a = "add"
	val () = println!("'", a, "' head is ", string_head(a) )
}
