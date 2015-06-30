staload "libats/ML/SATS/basis.sats"

datatype env(a:t@ype,b:t@ype) =
  | env(a,b) of (a,b)

fn a(i:int,j:int):int

fn startsWith(i:string, param:string):bool

fn subn(i:string, n:size_t):string


fn any_char(): string -<cloref1> option0(env(string,string))

fn {a,b:t0ype+} peg_or
  (this:a -<cloref1> option0(b),
   that:a -<cloref1> option0(b)): a -<cloref1> option0(b)

fn{a,b,c,d,e:t0ype+} peg_seq
  (this:a -<cloref1> option0(env(b, c)),
   that:c -<cloref1> option0(env(d, e))
  ):(a)-<cloref1>option0(env(env(b, d), e))  

fn{a,b,c,d,e:t0ype+} peg_seqr
  (this:a -<cloref1> option0(env(b, c)),
   that:c -<cloref1> option0(env(d, e))
  ):(a)-<cloref1>option0(env(d, e))

fn{a,b,c,d,e:t0ype+} peg_seql
  (this:a -<cloref1> option0(env(b, c)),
   that:c -<cloref1> option0(env(d, e))
  ):a -<cloref1> option0(env(b, e))

fn{a,b,c,d:t0ype+} peg_action
  (this: (a) -<cloref1> option0(env(b,c)),
   f: (b) -<cloref1> d
  ): (a) -<cloref1> option0(env(d,c))

exception ERROR of ()

fn{a,b,c,d:t0ype+} peg_action1
  (this:a->option0(env(b,c)),
   f:b-<cloref1>d
  ):a-<cloref1>option0(env(d,c))


fn{a,b:t0ype+} peg_opt
  (this:a-<cloref1>option0(env(b,a))
  ):a-<cloref1>option0(env(option0(b),a))
fn{a,b:t0ype+} peg_rep
  (this:a-<cloref1>option0(env(b,a))
  ): a -<cloref1> option0(env(list0(b),a))

fn{a,b:t0ype+} peg_rep1
  (this:a-<cloref1>option0(env(b,a))
  ):a-<cloref1> option0(env(list0(b),a))
fn{a,b,c:t0ype+} peg_notp
  (this:a-<cloref1>option0(env(b,c))
  ): a -<cloref1> option0(env(string,a))
fn peg_nstr(param:string):string-<cloref1> option0(env(string,string))
fn peg_range(c1:char,c2:char): (string)-<cloref1>option0(env(string,string))
fn peg_skip(): (string)-<cloref1> option0(env(string,string))
fn peg_str(param:string):(string)-<cloref1> option0(env(string,string))
