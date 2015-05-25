// lte_string_string.dats

#include "share/atspre_staload.hats"

implement main0() = {
	val a = "add"
	val b = "mul"
	val () = println!("'", a, "' <= '", b, "'? ", lte_string_string(a, b))
	val () = println!("'", a, "' <= '", b, "'? ", a <= b)
}

