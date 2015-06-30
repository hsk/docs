// patscc -DATS_MEMALLOC_LIBC -L$PATSHOME/ccomp/atslib/lib -latslib peg.dats syntax.dats parser.dats main.dats -o main; ./main
#include "share/atspre_staload.hats"

#include "./syntax.hats"
staload "./syntax.dats"
#include "./parser.hats"
staload "./parser.dats"

fun eval(e:e):int =
  case e of
  | Int(i) => i
  | Bin(e1,"+",e2) => eval(e1)+eval(e2)
  | Bin(e1,"-",e2) => eval(e1)-eval(e2)
  | Bin(e1,"*",e2) => eval(e1)*eval(e2)
  | Bin(e1,"/",e2) => eval(e1)/eval(e2)
  | Bin(e1,op1,e2) => eval(e1)

implement main0 () = {

  //val a = int_()("aaa")
  val- Some0(env(ii,s)) = int_()("1234")

  val () = println!("i=",ii)
  val- Some0(env(i,s)) = exp()(" 10 * 2 + 200 /  (15 - 5)")
  val () = println!("i=",i)
  val () = println!("i=",eval(i))

}
