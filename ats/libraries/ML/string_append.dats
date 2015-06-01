// string_append.dats
// patscc string_append.dats -DATS_MEMALLOC_LIBC -latslib -o string_append
#include "share/atspre_staload.hats"
staload "libats/ML/SATS/string.sats"
staload "libats/ML/DATS/string.dats"

implement main0() = {
  val Hello = "H"+"e"+"l"+"l"+"o"
  val () = print (Hello + ", world!\n")
}


