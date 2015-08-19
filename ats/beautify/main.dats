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

fun eval(env:env, e:e):int =
  case e of
  | Int(i) => i
  | Var(i) => list0_assoc_exn(env, i, lam(a,b)=>a=b)
  | Bin(e1,"+",e2) => eval(env, e1)+eval(env, e2)
  | Bin(e1,"-",e2) => eval(env, e1)-eval(env, e2)
  | Bin(e1,"*",e2) => eval(env, e1)*eval(env, e2)
  | Bin(e1,"/",e2) => eval(env, e1)/eval(env, e2)
  | Bin(e1,op1,e2) => eval(env, e1)
  | Let(i,e1,e2) =>
    let
      val r1 = eval(env, e1)
    in
      eval(cons0(@(i,r1), env), e2)
    end

implement main0 (argv, argc) = {
  val () = if (argv < 2) then {
    val () = println!("usage main filename")
  } else {
    val src = read_all(argc[1])
    val- Some0(res(i,s)) = exp(src)
    val () = println!("AST=",i)
    val () = println!(src,"=",eval(nil0(), i))
  }

}
