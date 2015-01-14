// while.dats

#include "share/atspre_staload.hats"

fun fact (n: int): int =
  if n > 0 then n * fact(n-1) else 1

fun fact2(n: int): int = let
  var n: int = n
  var res: int = 1
in
  while (n > 0) (res := res * n; n := n - 1); res
end

implement main0 () = {
	val () = println!(fact(10))
	val () = println!(fact2(10))
}
