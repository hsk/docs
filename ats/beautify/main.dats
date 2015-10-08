// patscc -DATS_MEMALLOC_LIBC -L$PATSHOME/ccomp/atslib/lib -latslib peg.dats syntax.dats parser.dats main2.dats -o main2; ./main2 a.txt

#include "peg.hats"
staload "peg.dats"

staload "syntax.sats"
staload "syntax.dats"

staload "parser.sats"
staload "parser.dats"
dynload "parser.dats"

staload "libats/ML/SATS/list0.sats"

typedef env = list0(@(string,int))

implement main0 (argv, argc) = {
  val () = if (argv < 2) then {
    val () = println!("usage main filename")
  } else {
    val src = read_all(argc[1])
    val- Some0(res(i,s)) = exp(src)
    val () = println!("AST=",i)
  }

}
