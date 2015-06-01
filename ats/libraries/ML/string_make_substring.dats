// string_make_substring.dats
// patscc string_make_substring.dats -DATS_MEMALLOC_LIBC -latslib -o string_make_substring
#include "share/atspre_staload.hats"
staload "libats/ML/SATS/string.sats"
staload "libats/ML/DATS/string.dats"

implement main0() = {
  val str = "abcdefg"
  val str2 = string_make_substring (str, g1int2uint(0), string_length(str))
  val () = assertloc (str = str2)
  val () = println!(str2)
}

