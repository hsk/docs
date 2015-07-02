// patscc -DATS_MEMALLOC_LIBC -L$PATSHOME/ccomp/atslib/lib -latslib peg.dats syntax.dats parser.dats main2.dats -o main2; ./main2 a.txt

#include "peg.hats"
staload "peg.dats"

staload "syntax.sats"
staload "syntax.dats"

staload "parser.sats"
staload "parser.dats"
dynload "parser.dats"

fun eval(e:e):int =
  case e of
  | Int(i) => i
  | Bin(e1,"+",e2) => eval(e1)+eval(e2)
  | Bin(e1,"-",e2) => eval(e1)-eval(e2)
  | Bin(e1,"*",e2) => eval(e1)*eval(e2)
  | Bin(e1,"/",e2) => eval(e1)/eval(e2)
  | Bin(e1,op1,e2) => eval(e1)

implement main0 (argv, argc) = {
  val () = if (argv < 2) then {
    val () = println!("usage main filename")
  } else {
    val src = read_all(argc[1])
    val- Some0(res(i,s)) = exp(src)
    val () = println!("i=",i)
    val () = println!("i=",eval(i))
  }

}
