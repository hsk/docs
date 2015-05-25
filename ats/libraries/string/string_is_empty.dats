// string_is_empty.dats

#include "share/atspre_staload.hats"

implement main0() = {
	val a = "add"
	val () = println!("'", a, "' is empty? ", string_is_empty(a) )
	val b = ""
	val () = println!("'", b, "' is empty? ", string_is_empty(b) )
}
