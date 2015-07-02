staload "libats/ML/SATS/basis.sats"

datatype res(a:t@ype,b:t@ype) =
  | res(a,b) of (a,b)

exception ERROR of ()

fn startsWith(i:string, param:string):bool
fn subn(i:string, n:size_t):string
val any_char:string -<cloref1> option0(res(string,string))

fn nstr(param:string):string-<cloref1> option0(res(string,string))
fn {a,b,c:t0ype+} or
  (this:a -<cloref1> option0(res(b,c)),
   that:a -<cloref1> option0(res(b,c))): a -<cloref1> option0(res(b,c))
overload / with or

fn {a,b,c,d,e:t0ype+} seq
  (this:a -<cloref1> option0(res(b, c)),
   that:c -<cloref1> option0(res(d, e))
  ):(a)-<cloref1>option0(res(res(b, d), e))
overload ~ with seq

fn {a,b,c,d,e:t0ype+} seqr
  (this:a -<cloref1> option0(res(b, c)),
   that:c -<cloref1> option0(res(d, e))
  ):(a)-<cloref1>option0(res(d, e))
overload ~> with seqr

fn {a,b,c,d,e:t0ype+} seql
  (this:a -<cloref1> option0(res(b, c)),
   that:c -<cloref1> option0(res(d, e))
  ):a -<cloref1> option0(res(b, e))
overload <~ with seql

fn {a,b,c,d:t0ype+} action
  (this: (a) -<cloref1> option0(res(b,c)),
   f: b -> d
  ): a -<cloref1> option0(res(d,c))
overload ^^ with action

fn {a,b,c,d:t0ype+} action1
  (this:a->option0(res(b,c)),
   f:b -> d
  ):a-<cloref1>option0(res(d,c))
overload ^?^ with action1

fn {a,b:t0ype+} opt
  (this:a-<cloref1>option0(res(b,a))
  ):a-<cloref1>option0(res(option0(b),a))
fn {a,b:t0ype+} rep
  (this:a-<cloref1>option0(res(b,a))
  ): a -<cloref1> option0(res(list0(b),a))
fn {a,b:t0ype+} rep1
  (this:a-<cloref1>option0(res(b,a))
  ):a-<cloref1> option0(res(list0(b),a))
fn {a,b,c:t0ype+} notp
  (this:a-<cloref1>option0(res(b,c))
  ): a -<cloref1> option0(res(string,a))
fn range(c1:char,c2:char): (string)-<cloref1>option0(res(string,string))
