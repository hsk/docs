#include "peg.hats"
//#define ATS_DYNLOADFLAG 0

implement startsWith(i:string, param:string):bool =
  let
    val ilen = string_length i
    val plen = string_length param
  in
    if plen > ilen then false
    else
      string_make_substring(i, i2sz 0, plen) = param
  end

implement subn(i:string, n:size_t):string =
  string_make_substring(i, n, (string_length i) - n)

implement any_char: string -<cloref1> option0(res(string,string)) =
  lam i =<cloref1>
    if(string_length i <= i2sz 0) then None0() else
    Some0(res(string_make_substring(i, i2sz 0, i2sz 1),subn(i, i2sz 1)))

implement nstr(param:string):string-<cloref1> option0(res(string,string)) =
  lam i =<cloref1>
    if startsWith(i, param)
    then Some0(res(param, subn(i, string_length(param))))
    else None0()

implement {a,b,c:t0ype+} or
  (this:a -<cloref1> option0(res(b,c)),
   that:a -<cloref1> option0(res(b,c))): a -<cloref1> option0(res(b,c)) =
  lam i =<cloref1>
    case this i of
    | None0() => that i
    | e       => e

implement {a,b,c,d,e:t0ype+} seq
  (this:a -<cloref1> option0(res(b, c)),
   that:c -<cloref1> option0(res(d, e))
  ):(a)-<cloref1>option0(res(res(b, d), e)) =
  lam i =<cloref1>
    case this i of
    | None0() => None0()
    | Some0(res(r1, rest1)) =>
      case that rest1 of
      | None0()               => None0()
      | Some0(res(r2, rest2)) => Some0(res(res(r1, r2), rest2))

implement {a,b,c,d,e:t0ype+} seqr
  (this:a -<cloref1> option0(res(b, c)),
   that:c -<cloref1> option0(res(d, e))
  ):(a)-<cloref1>option0(res(d, e)) =
  lam i =<cloref1>
    case this i of
    | None0()               => None0()
    | Some0(res(r1, rest1)) => that rest1

implement {a,b,c,d,e:t0ype+} seql
  (this:a -<cloref1> option0(res(b, c)),
   that:c -<cloref1> option0(res(d, e))
  ):a -<cloref1> option0(res(b, e)) =
  lam i =<cloref1>
    case (this ~ that) i of
    | None0()                  => None0()
    | Some0(res(res(b,c), rs)) => Some0(res(b, rs))

implement {a,b,c,d:t0ype+} action
  (this: (a) -<cloref1> option0(res(b,c)),
   f: b -> d
  ): a -<cloref1> option0(res(d,c)) =
  lam i =<cloref1>
    case this i of
    | Some0(res(r, rest)) => Some0(res(f r, rest))
    | None0()             => None0()

implement {a,b,c,d:t0ype+} action1
  (this:a->option0(res(b,c)),
   f:b -> d
  ):a-<cloref1>option0(res(d,c)) =
  lam i =<cloref1>
    try
      case this i of
      | Some0(res(r, rest)) => Some0(res(f r, rest))
      | None0()             => None0()
    with
      | ~ERROR() => None0()

implement {a,b:t0ype+} opt
  (this:a-<cloref1>option0(res(b,a))
  ):a-<cloref1>option0(res(option0(b),a)) =
  lam i =<cloref1>
    case this i of
    | None0()            => Some0(res(None0(), i))
    | Some0(res(p1, p2)) => Some0(res(Some0(p1), p2))

implement {a,b:t0ype+} rep
  (this:a-<cloref1>option0(res(b,a))
  ): a -<cloref1> option0(res(list0(b),a)) =
  lam i =<cloref1> 
    let
      fun loop(l, i) =
        case this i of
        | None0()             => Some0(res(list0_reverse l, i))
        | Some0(res(r, rest)) => loop(cons0(r,l), rest)
    in
      loop(nil0(), i)
    end

implement {a,b:t0ype+} rep1
  (this:a-<cloref1>option0(res(b,a))
  ):a-<cloref1> option0(res(list0(b),a)) =
  this ~ rep this
  ^^ begin lam(res(p1,p2)) =>
    cons0(p1,p2)
  end

implement {a,b,c:t0ype+} notp
  (this:a-<cloref1>option0(res(b,c))
  ): a -<cloref1> option0(res(string,a)) =
  lam i =<cloref1>
    case this i of
    | None0()  => Some0(res("", i))
    | _        => None0()

implement range(c1:char,c2:char): (string)-<cloref1>option0(res(string,string)) =
  lam i =<cloref1>
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
