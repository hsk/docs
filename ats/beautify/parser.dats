//#define ATS_DYNLOADFLAG 0
#include "parser.hats"
dynload "parsec.dats"

val comment =
  nstr "/*" ~> rep(notp(nstr "*/")~any_char) ~> nstr "*/"

val skip =
  rep(nstr " " / nstr "\r" / nstr "\n" / nstr "\t" / comment)
  ^^ begin lam _ =>
    ""
  end

fn str(param) =
  skip ~> nstr param

val zero =
  str "0"
  ^^ begin lam _ =>
    0
  end

val nonzero =
  range('1', '9')
  ^^ begin lam(a) =>
    $STDLIB.atoi a
  end

val digit =
  range('0', '9')
  ^^ begin lam a =>
    $STDLIB.atoi a
  end

val no =
  nonzero ~ rep(digit)
  ^^ begin lam(res(l, ls)) =>
    list0_foldleft<int><int>(ls, l, lam(a, b) => a * 10 + b)
  end

val int_ =
  skip ~> (no / zero)

//extern val exp : string -<cloref1> option0(res(e,string))

val fact = 
  int_
  ^^ begin lam e =>
    EInt e
  end
/ str "(" ~> (lam i=> exp i) <~ str ")"

val term =
  fact ~ rep((str "*" / str "/") ~ fact)
  ^^ begin lam (res(t,ts)) =>
    list0_foldleft (ts, t, lam(t1,res(op1, t2)) =>
      EBin(t1, op1, t2)
    )
  end

implement exp =
  term ~ rep((str "+" / str "-") ~ term)
  ^^ begin lam (res(t, ts)) =>
    list0_foldleft (ts, t, lam(t1, res(op1, t2)) =>
      EBin(t1, op1, t2)
    )
  end
