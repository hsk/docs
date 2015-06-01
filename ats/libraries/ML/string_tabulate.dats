// string_tabulate.dats
// patscc string_tabulate.dats -DATS_MEMALLOC_LIBC -latslib -o string_tabulate

#include "share/atspre_staload.hats"
staload "libats/ML/SATS/string.sats"
staload "libats/ML/DATS/string.dats"

implement main0 () = {
  val ds = string_tabulate (i2sz(10), lam i => $UN.cast{charNZ}('0'+sz2i(i)))
  val out = stdout_ref
  val () = fprintln! (out, "digits = ", ds)
  val () = string_foreach (ds+ds, lam c => fprint(out, c))
  val () = fprint_newline (out)
}

