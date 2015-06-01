// string_copy.dats
// patscc string_copy.dats -DATS_MEMALLOC_LIBC -latslib -o string_copy

#include "share/atspre_staload.hats"
staload "libats/ML/SATS/string.sats"
staload "libats/ML/DATS/string.dats"
implement main0 () = { 
  val str = "abcdefg" 
  val ()  = println!(string_copy(str))
}
