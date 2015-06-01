// string_implode.dats
// patscc string_implode.dats -DATS_MEMALLOC_LIBC -latslib -o string_implode

#include "share/atspre_staload.hats"
staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/DATS/list0.dats"
staload "libats/ML/SATS/string.sats"
staload "libats/ML/DATS/string.dats"

implement main0 () = {
  val str = "abcdefg"
  val cs = string_explode (str)
  val str2 = string_implode (cs)
  val () = assertloc (str = str2)
}

