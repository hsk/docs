// string_sing.dats
// patscc string_sing.dats -DATS_MEMALLOC_LIBC -latslib -o string_sing

#include "share/atspre_staload.hats"
staload "libats/ML/SATS/string.sats"
staload "libats/ML/DATS/string.dats"
implement main0 () = {
  val ()  = print(string_sing('a')+"\n")
}
