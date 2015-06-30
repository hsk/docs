#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0
#include "./peg.hats"

implement a(i:int,j:int):int = i+j

implement startsWith(i:string, param:string):bool =
  let
    val ilen = string_length (i)
    val (plen:size_t) = string_length (param)
  in
    if plen > ilen then false
    else
      string_make_substring(i, (i2sz)0, plen) = param
  end

implement subn(i:string, n:size_t):string =
  string_make_substring(i, n, (string_length i) - n)

implement any_char(): string -<cloref1> option0(env(string,string)) =
  lam(i:string) =<cloref1>
    if(string_length i > (i2sz)0)
    then
      let
        val s1 = string_make_substring(i, (i2sz)0, (i2sz)1)
        val s2 = subn(i, (i2sz)1) 
      in
        Some0(env(s1,s2))
      end
    else None0()

implement peg_nstr(param:string):string-<cloref1> option0(env(string,string)) =

  lam(i:string): option0(env(string,string)) =<cloref1>
  let
    val ilen = string_length (i)
    val (plen:size_t) = string_length (param)
    val start = if plen > ilen then false
    else
      string_make_substring(i, (i2sz)0, plen) = param
  in
    if start
    then
      let
        val n = string_length(param)
        val i2 = string_make_substring(i, n, (string_length i) - n)
      in
        Some0(env(param, i2))
      end
    else None0()
  end

implement {a,b:t0ype+} peg_or
  (this:a -<cloref1> option0(b),
   that:a -<cloref1> option0(b)): a -<cloref1> option0(b) =
  lam(i)=<cloref1>
    begin case this(i) of
    | Some0(e) => Some0(e)
    | None0() => that(i)
    end

implement {a,b,c,d,e:t0ype+} peg_seq
  (this:a -<cloref1> option0(env(b, c)),
   that:c -<cloref1> option0(env(d, e))
  ):(a)-<cloref1>option0(env(env(b, d), e))=
  lam(i) =<cloref1>
    case this(i) of
    | Some0(env(r1, rest1)) =>
      (case that (rest1) of
      | Some0(env(r2, rest2)) => Some0(env(env(r1, r2), rest2))
      | None0() => None0()
      )
    | None0() => None0()

implement {a,b,c,d,e:t0ype+} peg_seqr
  (this:a -<cloref1> option0(env(b, c)),
   that:c -<cloref1> option0(env(d, e))
  ):(a)-<cloref1>option0(env(d, e))=
  lam(i) =<cloref1>
    case this(i) of
    | Some0(env(r1, rest1)) => that (rest1)
    | None0() => None0()


implement {a,b,c,d,e:t0ype+} peg_seql
  (this:a -<cloref1> option0(env(b, c)),
   that:c -<cloref1> option0(env(d, e))
  ):a -<cloref1> option0(env(b, e))=
  lam(i:a) =<cloref1>
    case (this <~> that)(i) of
    | None0() => None0()
    | Some0(env(env(b,_), rs)) => Some0(env(b, rs))

implement {a,b,c,d:t0ype+} peg_action
  (this: a -<cloref1> option0(env(b,c)),
   f: b -<cloref1> d
  ): (a) -<cloref1> option0(env(d,c)) =
  lam(i) =<cloref1>
    case this (i) of
    | Some0(env(r, rest)) => Some0(env(f(r), rest))
    | None0() => None0()

implement{a,b,c,d:t0ype+} peg_action1
  (this:a->option0(env(b,c)),
   f:b-<cloref1>d
  ):a-<cloref1>option0(env(d,c)) =
  lam(i:a) =<cloref1>
    try
      case this (i) of
      | Some0(env(r, rest)) => Some0(env(f(r), rest))
      | None0() => None0()
    with
      | ~ERROR() => None0()

implement{a,b:t0ype+} peg_opt
  (this:a-<cloref1>option0(env(b,a))
  ):a-<cloref1>option0(env(option0(b),a)) =
  lam(i:a) =<cloref1>
    case this (i) of
    | Some0(env(p1, p2)) => Some0(env(Some0(p1), p2))
    | None0()            => Some0(env(None0(), i))

implement{a,b:t0ype+} peg_rep
  (this:a-<cloref1>option0(env(b,a))
  ): a -<cloref1> option0(env(list0(b),a)) =
  lam(i)=<cloref1> 
    let
      fun loop(l:list0(b), i:a) : option0(env(list0(b),a)) =
        case this (i) of
        | None0() => Some0(env(list0_reverse(l), i))
        | Some0(env(r, rest)) => loop(cons0(r,l),rest)
    in
      loop(nil0(),i)
    end

implement{a,b:t0ype+} peg_rep1
  (this:a-<cloref1>option0(env(b,a))
  ):a-<cloref1> option0(env(list0(b),a)) =
  (this <~> peg_rep(this)) ^^ begin lam(env(p1,p2)) =<cloref1>
    cons0(p1,p2)
  end

implement{a,b,c:t0ype+} peg_notp
  (this:a-<cloref1>option0(env(b,c))
  ): a -<cloref1> option0(env(string,a)) =
  lam(i) =<cloref1>
    case this i of
    | Some0(_) => None0()
    | None0() => Some0(env("", i))

implement peg_range(c1:char,c2:char): (string)-<cloref1>option0(env(string,string)) =
  lam(i:string) =<cloref1>
    if(string_length(i) <= (i2sz)0)
    then None0()
    else
     let
      val c3 = strarr_make(i)
      val c:char = c3[0]
     in
      if (c1 <= c) && (c <= c2) then
        Some0(env(string_make_substring(i, (i2sz)0, (i2sz)1), subn(i, (i2sz)1)))
      else None0()
     end

implement peg_skip(): (string)-<cloref1> option0(env(string,string)) =
  lam(i)=<cloref1>
    let
      fn skip2():(string)-<cloref1> option0(env(string,string)) =
        peg_nstr(" ") <|> peg_nstr("\r") <|> peg_nstr("\n") <|> peg_nstr("\t")
      fun loop(i:string) =
        case skip2()(i) of
        | None0() => Some0(env("", i))
        | Some0(env(_, rest1)) => loop(rest1)
    in
      loop(i)
    end

implement peg_str(param:string):(string)-<cloref1> option0(env(string,string))=
  lam(i)=<cloref1>
      peg_nstr(param)(
        case peg_skip()(i) of
        | None0() => i
        | Some0(env(a,b)) => b
      )

/*
val () = {
  val () = println!("test parser")
  val () = 
    case peg_str("1") (" 1+2") of
    | None0() => println!("none")
    | Some0(env(a,b)) => println!("some ",a," ",b)

  val () =
    case peg_rep(peg_range('0','9'))("3") of
    | None0() => println!("none")
    | Some0(env(a,b)) => println!("some ",a," ",b)

}
*/
