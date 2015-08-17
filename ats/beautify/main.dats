// patscc -DATS_MEMALLOC_LIBC -L$PATSHOME/ccomp/atslib/lib -latslib parsec.dats ast.dats parser.dats main.dats -o main; ./main a.txt

#include "parsec.hats"
staload "parsec.dats"

staload "ast.sats"
staload "ast.dats"

staload "parser.sats"
staload "parser.dats"
dynload "parser.dats"

fun eval(e:e):int =
  case e of
  | EInt(i) => i
  | EBin(e1,"+",e2) => eval(e1)+eval(e2)
  | EBin(e1,"-",e2) => eval(e1)-eval(e2)
  | EBin(e1,"*",e2) => eval(e1)*eval(e2)
  | EBin(e1,"/",e2) => eval(e1)/eval(e2)
  | EBin(e1,op1,e2) => eval(e1)
  | _ => 0
implement main0 (argv, argc) = {
  val- Some0(res(i,s)) = nstr("str")("str")
//  val- Some0(res(i,s)) = any_char("str")
  //val- Some0(res(i,s)) = zero("0")
  val- Some0(res(i,s)) = exp("/*abc*/ 10 * 2 + 200 /  (15 - 5)")
  val () = println!("i=",i)
  val () = println!("i=",eval(i))

  val () = if (argv > 1) then {
  	val src = read_all(argc[1])
  	val () = println!(src)
	  val- Some0(res(i,s)) = exp(src)
	  val () = println!("i=",i)
	  val () = println!("i=",eval(i))
  }

}
