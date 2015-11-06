// patscc -DATS_MEMALLOC_LIBC  -I/usr/include/malloc/ -L$PATSHOME/ccomp/atslib/lib -latslib malloc.dats -o malloc ; ./malloc

#include "share/atspre_staload.hats"

staload "libc/SATS/malloc.sats"
staload "libc/SATS/stdlib.sats"

val () = {
	val N = g1i2u (1024)
	val (pfopt | p) = malloc_libc (N)
	val () = assertloc (p > 0)
	prval malloc_libc_v_succ (pfat, pfgc) = pfopt
	val () = mfree_libc (pfat, pfgc | p)
}

implement main0 () = ()
