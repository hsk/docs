// string_isnot_empty.dats

#include "share/atspre_staload.hats"

implement main0() = {
	val a = "add"
	val () = println!("'", a, "' is not empty? ", string_isnot_empty(a) )
	val b = ""
	val () = println!("'", b, "' is not empty? ", string_isnot_empty(b) )
}
