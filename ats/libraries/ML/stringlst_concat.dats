// stringlst_concat.dats
// patscc stringlst_concat.dats -DATS_MEMALLOC_LIBC -latslib -o stringlst_concat

#include "share/atspre_staload.hats"
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/DATS/list0.dats"
staload "libats/ML/SATS/string.sats"
staload "libats/ML/DATS/string.dats"

implement main0 () = {
  val Hello = stringlst_concat ((list0)$arrpsz{string}("H","e","l","l","o"))
  val () = print (Hello + ", world!\n")
}

