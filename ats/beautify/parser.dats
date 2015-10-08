//#define ATS_DYNLOADFLAG 0
#include "parser.hats"
dynload "peg.dats"

val comment =
  nstr "/*" ~> rep(notp(nstr "*/")~any_char) ~> nstr "*/"

val skip =
  rep(nstr " " / nstr "\r" / nstr "\n" / nstr "\t" / comment)
  ^^ begin lam _ =>
    ""
  end

fn str p =
  skip ~> nstr p

val zero =
  str "0"

val nonzero =
  range('1', '9')

val digit =
  range('0', '9')

val no =
  nonzero ~ rep(digit)
  ^^ begin lam(res(l, ls)) =>
    stringlst_concat(cons0(l,ls))
  end

val int_ =
  skip ~> (no / zero)

val upper =
  range('A','Z')

val lower =
  range('a','z')

val alpha =
  upper / lower

val ident =
  skip ~> (alpha / nstr "_") ~ rep(alpha / nstr "_" / digit)
  ^^ begin lam(res(a,ls)) =>
    stringlst_concat(cons0(a,ls))
  end

val fact =
  int_
  ^^ begin lam e =>
    Str e
  end
/ str "(" ~> r exp <~ str ")"
/ str "let" ~> ident ~ 
  (str "=" ~> r exp) ~ (str "in" ~> r exp)
  ^^ begin lam(res(i,res(a,b))) =>
    Str(i)
  end
/ ident
  ^^ begin lam e =>
    Str e
  end

val term =
  fact ~ rep((str "*" / str "/") ~ fact)
  ^^ begin lam (res(t,ts)) =>
    list0_foldleft (ts, t, lam(t1,res(op1, t2)) =>
      Lst(list0_cons(t1,list0_cons(Str(op1),list0_cons(t2, nil0))))
    )
  end

implement exp =
  term ~ rep((str "+" / str "-") ~ term)
  ^^ begin lam (res(t, ts)) =>
    list0_foldleft (ts, t, lam(t1,res(op1, t2)) =>
      Lst(list0_cons(t1,list0_cons(Str(op1),list0_cons(t2, nil0))))
    )
  end
