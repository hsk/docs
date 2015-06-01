// itoa.dats
// patscc itoa.dats -DATS_MEMALLOC_LIBC -latslib -o itoa

#include "share/atspre_staload.hats"
staload "libats/ML/SATS/string.sats"
staload "libats/ML/DATS/string.dats"
implement main0 () = {
  val ()  = print(itoa(10)+"\n")
}
