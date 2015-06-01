// string_explode.dats
// patscc string_explode.dats -DATS_MEMALLOC_LIBC -latslib -o string_explode

#include "share/atspre_staload.hats"
staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/DATS/list0.dats"
staload "libats/ML/SATS/string.sats"
staload "libats/ML/DATS/string.dats"

fun print_lst (xs: list0 (char)): void =
  case+ xs of
  | list0_nil () => ()
  | list0_cons (x, xs) =>
    {
      val () = println!(x)
      val () = print_lst (xs)
    }

implement main0 () = {
  val str = "abcdefg"
  val cs = string_explode (str)
  val () = assertloc (string_length (str) = g0i2u(list0_length (cs)))
  val () = print_lst(cs)
}
