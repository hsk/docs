// patscc -DATS_MEMALLOC_LIBC -L$PATSHOME/ccomp/atslib/lib -latslib file.dats -o file; ./file

#include "share/atspre_staload.hats"

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload _ = "libats/ML/DATS/list0.dats"
staload "libats/ML/SATS/filebas.sats"

val () = {
  val-~Some_vt(filr) = fileref_open_opt ("./file.dats", file_mode_r)
  val lines = fileref_get_lines_stringlst (filr)
  
  fun loop(out: FILEref, xs: list0 (string), i: int) : void =
    case+ xs of
    | nil0 () => ()
    | cons0 (x, xs) =>(fprintln! (out, "line(", i, ") = ", x); loop (out, xs, i+1))
  val () = loop (stdout_ref, lines, 1)
}

implement main0 () = {}
