// for.dats

#include "share/atspre_staload.hats"

fun fact (n: int): int =
  if n > 0 then n * fact(n-1) else 1

fun fact3
  (n: int): int = let
  var i: int
  var res: int = 1
in
  for (i := n; i > 0; i := i-1) res := res * i; res
end

implement main0 () = {
	val () = println!(fact(10))
	val () = println!(fact3(10))
}

