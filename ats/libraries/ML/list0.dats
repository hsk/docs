// patscc -DATS_MEMALLOC_LIBC -L$PATSHOME/ccomp/atslib/lib -latslib list0.dats -o list0; ./list0
#include "share/atspre_staload.hats"
staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/DATS/list0.dats"
#define :: cons0

fun printls(xs: list0(int)) : void =
  begin case+ xs of
    | x :: xs =>
      begin
        println! x;
        printls xs
      end
    | _ => ()
  end

implement main0 () = {
  val () = printls(nil0())
  val () = printls(cons0(1,nil0()))
  val () = printls(list0_of_list($list(0, 2, 4, 6, 8)))
  val () = printls(1::2::3::nil0())
}
