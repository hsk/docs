#include "peg.hats"
//#define ATS_DYNLOADFLAG 0
macdef r a = (lam i =<cloref1> ,(a) i)

implement startsWith(i, param) =
  let
    val ilen = string_length i
    val plen = string_length param
  in
    if plen > ilen then false
    else
      string_make_substring(i, i2sz 0, plen) = param
  end

implement subn(i, n) =
  string_make_substring(i, n, (string_length i) - n)

implement any_char = lam i =>
  if(string_length i <= i2sz 0) then None0() else
  Some0(res(string_make_substring(i, i2sz 0, i2sz 1),subn(i, i2sz 1)))

implement nstr param = lam i =>
  if startsWith(i, param)
  then Some0(res(param, subn(i, string_length(param))))
  else None0()

implement {a,b,c:t0ype+} or(this, that) = lam i =>
  case this i of
  | None0() => that i
  | e       => e

implement {a,b,c,d,e:t0ype+} seq(this, that) = lam i =>
  case this i of
  | None0() => None0()
  | Some0(res(r1, rest1)) =>
    case that rest1 of
    | None0()               => None0()
    | Some0(res(r2, rest2)) => Some0(res(res(r1, r2), rest2))

implement {a,b,c,d,e:t0ype+} seqr(this, that) = lam i =>
  case this i of
  | None0()               => None0()
  | Some0(res(r1, rest1)) => that rest1

implement {a,b,c,d,e:t0ype+} seql(this, that) = lam i =>
  case (this ~ that) i of
  | None0()                  => None0()
  | Some0(res(res(b,c), rs)) => Some0(res(b, rs))

implement {a,b,c,d:t0ype+} action(this, f) = lam i =>
  case this i of
  | Some0(res(r, rest)) => Some0(res(f r, rest))
  | None0()             => None0()

implement {a,b,c,d:t0ype+} action1(this, f) = lam i =>
  try
    case this i of
    | Some0(res(r, rest)) => Some0(res(f r, rest))
    | None0()             => None0()
  with
    | ~ERROR() => None0()

implement {a,b:t0ype+} opt this = lam i =>
  case this i of
  | None0()            => Some0(res(None0(), i))
  | Some0(res(p1, p2)) => Some0(res(Some0(p1), p2))

implement {a,b:t0ype+} rep this = lam i => 
  let
    fun loop(l, i) =
      case this i of
      | None0()             => Some0(res(list0_reverse l, i))
      | Some0(res(r, rest)) => loop(cons0(r,l), rest)
  in
    loop(nil0(), i)
  end

implement {a,b:t0ype+} rep1 this =
  this ~ rep this
  ^^ begin lam(res(p1,p2)) =>
    cons0(p1,p2)
  end

implement {a,b,c:t0ype+} notp this = lam i =>
  case this i of
  | None0()  => Some0(res("", i))
  | _        => None0()

implement range(c1,c2) = lam i =>
  if string_length i <= i2sz 0
  then None0()
  else
    let
      val c3 = strarr_make i
      val c:char = c3[0]
    in
      if (c1 <= c) && (c <= c2) then
        Some0(res(string_make_substring(i, i2sz 0, i2sz 1), subn(i, i2sz 1)))
      else None0()
    end
