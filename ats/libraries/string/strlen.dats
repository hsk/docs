// strlen.dats

#include "share/atspre_staload.hats"

implement main0() = {
	val a = "add"
	val () = println!("'", a, "' length = ", strlen(a))
	val () = println!("'", a, "' length = ", string_length(a))
}

