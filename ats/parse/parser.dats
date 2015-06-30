// patscc -DATS_MEMALLOC_LIBC -L$PATSHOME/ccomp/atslib/lib -latslib peg.dats syntax.dats parser.dats -o main; ./main

#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0
#include "./peg.hats"
staload "./peg.dats"
#include "./syntax.hats"
#include "./parser.hats"

implement int_ (): string -<cloref1> option0(env(int,string)) =
  lam(i:string) =<cloref1>
    let
      fn zero() = (peg_str("0")) ^^ begin lam(a) =<cloref1>
        0
      end

      fn nonzero() = (peg_range('1', '9')) ^^ begin lam(a) =<cloref1>
        $STDLIB.atoi(a)
      end
      fn digit() = (peg_range('0', '9')) ^^ begin lam(a) =<cloref1>
        $STDLIB.atoi(a)
      end
      fn no() = (nonzero() <~> peg_rep(digit())) ^^ begin lam(env(l:int,ls:list0(int))):int =<cloref1>
        list0_foldleft<int><int>(ls,l,lam(a,b)=>a*10+b)
      end
      fn parse() = peg_skip() ~> (
        no() <|>
        zero()
      )
    in
      parse()(i)
    end
/*
val () = {
  val ls:list0(int) = list0_of_list($list(1,2,5,6))
  val l = list0_foldleft<int><int>(ls,0,lam(a,b)=>a*10+b)
  val () = println!("l=",l)
  val- Some0(env(i,s)) = int_()("1234")
  val () = println!("i=",i)
}
*/

implement fact(): string -<cloref1> option0(env(e,string)) =
  begin
    ((int_()) ^^ (lam (e) => Int(e)))
    <|> begin lam(i)=>
        (peg_str ("(") ~> exp() <~ peg_str(")"))(i)
    end
  end

implement term() : string -<cloref1> option0(env(e,string)) =
  (fact() <~> peg_rep( (peg_str("*") <|> peg_str("/")) <~> fact())) ^^ begin lam (env(t,ts)) =>
    list0_foldleft (ts, t, begin lam(t1,env(op1, t2)) =>
      Bin(t1, op1, t2)
    end)
  end

implement exp() : string -<cloref1> option0(env(e,string)) =
  (term() <~> peg_rep( (peg_str("+") <|> peg_str("-")) <~> term())) ^^ begin lam (env(t,ts)) =>
    list0_foldleft (ts, t, begin lam(t1,env(op1, t2)) =>
      Bin(t1, op1, t2)
    end)
  end
/*
val () = {
  val- Some0(env(i,s)) = exp()(" 1 * 2 + 20 /  ( 30 + 2)")
  val () = println!("i=",i)
}
*/