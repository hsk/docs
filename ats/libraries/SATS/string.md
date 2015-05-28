# string

HX: a string is a null-terminated arrayref of characters


## tk

```
sortdef tk = tkind
```

#### example

```
```

#### output

```
```

## SHR

```
typedef SHR(a:type) = a // for commenting purpose
```

#### example

```
```

#### output

```
```

## NSH

```
typedef NSH(a:type) = a // for commenting purpose
```

#### example

```
```

#### output

```
```

## stringLt

```
typedef stringLt (n:int) = [k:nat | k < n] string (k)
typedef stringLte (n:int) = [k:nat | k <= n] string (k)
typedef stringGt (n:int) = [k:int | k > n] string (k)
typedef stringGte (n:int) = [k:int | k >= n] string (k)
typedef stringBtw (m:int, n:int) = [k:int | m <= k; k < n] string (k)
typedef stringBtwe (m:int, n:int) = [k:int | m <= k; k <= n] string (k)
```

#### example

```
```

#### output

```
```

## stringlst

```
typedef stringlst = List0 (string)
```

#### example

```
```

#### output

```
```

## stringlst\_vt

```
vtypedef stringlst_vt = List0_vt (string)
```


#### example

```
```

#### output

```
```

## string\_index\_p

```
dataprop
string_index_p
(
  n: int, int(*i*), int(*c*)
) =
  | string_index_p_eqz (n, n, 0)
  | {i:int | n > i}
    {c:int8 | c != 0}
    string_index_p_neqz (n, i, c)
```

# StringSubscriptExn

```
exception StringSubscriptExn of ((*void*))
```

#### example

```
```

#### output

```
```

## lemma\_string\_param

```
praxi
lemma_string_param{n:int}(string n): [n >= 0] void
```

#### example

```
```

#### output

```
```

## string2ptr

```
castfn
string2ptr (x: string):<> Ptr1
overload ptrcast with string2ptr
```

#### example

```
```

#### output

```
```

## string1\_of\_string0

```
//
// HX: [string2string] = [string1_of_string0]
//
```

#### example

```
```

#### output

```
```

## g0ofg1\_string g1ofg0\_string

```
castfn g0ofg1_string (x: String):<> string
castfn g1ofg0_string (x: string):<> String0
overload g0ofg1 with g0ofg1_string // index-erasing
overload g1ofg0 with g1ofg0_string // index-inducing
```

#### example

```
```

#### output

```
```

## string\_sing

```
fun{}
string_sing (c: charNZ):<!wrt> strnptr (1)
```

#### example

```
// string_sing.dats
// patscc -DATS_MEMALLOC_LIBC string_sing.dats 
#include "share/atspre_staload.hats"

implement main0() = {
  val a = 'a'
  val () = println!("'", a, "' sing is ", strnptr2string(string_sing(a)))
}
```

#### output

```
'a' sing is a
```

## string\_is\_empty

fun{
} string\_is\_empty{n:int} (str: string(n)):<> bool(n==0)

#### example

```
// string_is_empty.dats

#include "share/atspre_staload.hats"

implement main0() = {
  val a = "add"
  val () = println!("'", a, "' is empty? ", string_is_empty(a) )
  val b = ""
  val () = println!("'", b, "' is empty? ", string_is_empty(b) )
}
```

#### output

```
'add' is empty? false
'' is empty? true
```

## string\_isnot\_empty

fun{
} string\_isnot\_empty{n:int} (str: string(n)):<> bool(n > 0)

#### example

```
// string_isnot_empty.dats

#include "share/atspre_staload.hats"

implement main0() = {
  val a = "add"
  val () = println!("'", a, "' is not empty? ", string_isnot_empty(a) )
  val b = ""
  val () = println!("'", b, "' is not empty? ", string_isnot_empty(b) )
}
```

#### output

```
```

# string\_is\_atend


## string\_is\_atend\_size

```
fun{}
string_is_atend_size
  {n:int}{i:nat | i <= n}
  (s: string (n), i: size_t (i)):<> bool (i==n)
```

#### example

```
```

#### output

```
```

## string\_is\_atend\_gint

```
fun{tk:tk}
string_is_atend_gint
  {n:int}{i:nat | i <= n}
  (s: string (n), i: g1int (tk, i)):<> bool (i==n)

overload string_is_atend with string_is_atend_gint
```

#### example

```
```

#### output

```
```

## string\_is\_atend\_guint

```
fun{tk:tk}
string_is_atend_guint
  {n:int}{i:nat | i <= n}
  (s: string (n), i: g1uint (tk, i)):<> bool (i==n)

overload string_is_atend with string_is_atend_guint
```

#### example

```
```

#### output

```
```

## string\_isnot\_atend

```
macdef
string_isnot_atend
  (string, index) = ~string_is_atend (,(string), ,(index))
```

#### example

```
```

#### output

```
```

## string\_head

```
fun{
} string_head{n:pos} (str: string(n)):<> charNZ
```

#### example

```
// string_head.dats

#include "share/atspre_staload.hats"

implement main0() = {
  val a = "add"
  val () = println!("'", a, "' head is ", string_head(a) )
}
```

#### output

```
'add' head is a
```

## string\_tail

```
fun{
} string_tail{n:pos} (str: string(n)):<> string(n-1)
```

#### example

```
// string_tail.dats

#include "share/atspre_staload.hats"

implement main0() = {
  val a = "add"
  val () = println!("'", a, "' tail is ", string_tail(a))
}
```

#### output

```
'add' tail is dd
```

## string\_get\_at\_size

```
fun{}
string_get_at_size
  {n:int}{i:nat | i < n}
  (s: string (n), i: size_t (i)):<> charNZ
```

#### example

```
```

#### output

```
```

## string\_get\_at\_gint

fun{tk:tk}
string\_get\_at\_gint
  {n:int}{i:nat | i < n}
  (s: string (n), i: g1int (tk, i)):<> charNZ

#### example

```
```

#### output

```
```

## string\_get\_at\_guint


fun{tk:tk}
string\_get\_at\_guint
  {n:int}{i:nat | i < n}
  (s: string (n), i: g1uint (tk, i)):<> charNZ

#### example

```
```

#### output

```
```

# symintr string\_get\_at

#### example

```
```

#### output

```
```

## 

overload string\_get\_at with string\_get\_at\_size of 1

#### example

```
```

#### output

```
```

## 

overload string\_get\_at with string\_get\_at\_gint of 0

#### example

```
```

#### output

```
```

## 

overload string\_get\_at with string\_get\_at\_guint of 0

#### example

```
```

#### output

```
```

## 

fun{}
string\_test\_at\_size
  {n:int}{i:nat | i <= n}
  (s: string (n), i: size\_t (i)):<> [c:int] (string\_index\_p (n, i, c) | char (c))

#### example

```
```

#### output

```
```

## 

fun{tk:tk}
string\_test\_at\_gint
  {n:int}{i:nat | i <= n}
  (s: string (n), i: g1int (tk, i)):<> [c:int] (string\_index\_p (n, i, c) | char (c))

#### example

```
```

#### output

```
```

## 

fun{tk:tk}
string\_test\_at\_guint
  {n:int}{i:nat | i <= n}
  (s: string (n), i: g1uint (tk, i)):<> [c:int] (string\_index\_p (n, i, c) | char (c))

#### example

```
```

#### output

```
```

## 

symintr string\_test\_at

#### example

```
```

#### output

```
```

## 

```
overload string_test_at with string_test_at_size of 1
overload string_test_at with string_test_at_gint of 0
overload string_test_at with string_test_at_guint of 0
```

#### example

```
```

#### output

```
```

## lt\_string\_string

```
fun lt_string_string
  (x1: string, x2: string):<> bool = "mac#%"

overload < with lt_string_string
```

#### example

```
```

#### output

```
```

## lte\_string\_string

```
fun lte_string_string
  (x1: string, x2: string):<> bool = "mac#%"

overload <= with lte_string_string
```

#### example

```
// lte_string_string.dats

#include "share/atspre_staload.hats"

implement main0() = {
        val a = "add"
        val b = "mul"
        val () = println!("'", a, "' <= '", b, "'? ", lte_string_string(a, b))
        val () = println!("'", a, "' <= '", b, "'? ", a <= b)
}
```

#### output

```
'add' <= 'mul'? true
'add' <= 'mul'? true
```

## gt\_string\_string

```
fun gt_string_string
  (x1: string, x2: string):<> bool = "mac#%"

overload > with gt_string_string
```

#### example

```
```

#### output

```
```

## gte\_string\_string

```
fun gte_string_string
  (x1: string, x2: string):<> bool = "mac#%"

overload >= with gte_string_string
```

#### example

```
```

#### output

```
```

## eq\_string\_string

```
fun eq_string_string
  (x1: string, x2: string):<> bool = "mac#%"

overload = with eq_string_string
```

#### example

```
```

#### output

```
```

## neq\_string\_string

```
fun neq_string_string
  (x1: string, x2: string):<> bool = "mac#%"

overload != with neq_string_string
overload <> with neq_string_string
```

#### example

```
```

#### output

```
```

## 

```
fun compare_string_string
  (x1: string, x2: string):<> Sgn = "mac#%"

overload compare with compare_string_string
```

#### example

```
```

#### output

```
```

## strcmp

```
fun{
} strcmp (x1: string, x2: string):<> int
```

#### example

```
```

#### output

```
```

## strintcmp

```
fun{
} strintcmp
  {n1,n2:int | n2 >=0}
  (x1: string n1, n2: int n2):<> int(sgn(n1-n2))
```

#### example

```
```

#### output

```
```

## strlencmp

```
fun{
} strlencmp
  {n1,n2:int}
  (x1: string n1, x2: string n2):<> int(sgn(n1-n2))
```

#### example

```
```

#### output

```
```

## string\_make\_list

```
fun{}
string_make_list
  {n:int} (cs: list(charNZ, n)):<!wrt> strnptr (n)
```

#### example

```
// string_make_list.dats
// patscc -DATS_MEMALLOC_LIBC string_make_list.dats 
#include "share/atspre_staload.hats"

implement main0() = {
  val xs = $list{char}('a','b','c')

  val () = println!("'", xs, "' string is ", strnptr2string(string_make_list(xs)))
}
```

#### output

```
```

## string\_make\_listlen

```
fun{}
string_make_listlen
  {n:int} (cs: list(charNZ, n), n: int n):<!wrt> strnptr (n)
```

#### example

```
```

#### output

```
```

## string\_make\_rlist

```
fun{
} string_make_rlist
  {n:int} (cs: list(charNZ, n)):<!wrt> strnptr (n)
```

#### example

```
```

#### output

```
```

## string\_make\_rlistlen

```
fun{
} string\_make\_rlistlen
  {n:int} (cs: list(charNZ, n), n: int n):<!wrt> strnptr (n)
```

#### example

```
```

#### output

```
```

## 

```
fun{
} string\_make\_substring
  {n:int}{st,ln:nat | st+ln <= n}
  (str: string (n), st: size\_t st, ln: size\_t ln):<!wrt> strnptr (ln)
```

#### example

```
```

#### output

```
```

## 

```
fun print\_string (x: string): void = "mac#%"
```

#### example

```
```

#### output

```
```

## 

```
fun prerr\_string (x: string): void = "mac#%"
```

#### example

```
```

#### output

```
```

## 

```
fun fprint\_string (out: FILEref, x: string): void = "mac#%"
```

#### example

```
```

#### output

```
```

## 

```
fun fprint\_substring
  {n:int}{st,ln:nat | st+ln <= n}
(
  out: FILEref, str: string(n), st: size\_t(st), ln: size\_t(ln)
) : void = "mac#%" // end of [fprint\_substring]
```

#### example

```
```

#### output

```
```

## 

```
fun{
} strchr{n:int}
  (str: string (n), c0: char):<> ssizeBtwe (~1, n)
```

#### example

```
```

#### output

```
```

## 

```
fun{
} strrchr{n:int}
  (str: string (n), c0: char):<> ssizeBtwe (~1, n)
```

#### example

```
```

#### output

```
```

## 

```
fun{
} strstr{n:int}
  (haystack: string (n), needle: string):<> ssizeBtw (~1, n)
```

#### example

```
```

#### output

```
```

## 

```
fun{
} strspn{n:int} // spanning
  (str: string (n), accept: string):<> sizeLte (n)
```

#### example

```
```

#### output

```
```

## 

```
fun{
} strcspn{n:int} // complement spanning
  (str: string (n), accept: string):<> sizeLte (n)
```

#### example

```
```

#### output

```
```

## string\_index

```
fun{
} string\_index{n:int}
  (str: string (n), c0: charNZ):<> ssizeBtw (~1, n)
```

#### example

```
```

#### output

```
```

## string\_rindex

```
fun{
} string\_rindex{n:int}
  (str: string (n), c0: charNZ):<> ssizeBtw (~1, n)
```

#### example

```
```

#### output

```
```

## string\_length

```
fun{
} string0\_length
  (x: NSH(string)):<> size\_t
fun{
} string1\_length
  {n:int} (x: NSH(string(n))):<> size\_t(n)

overload strlen with string0\_length of 0
overload strlen with string1\_length of 10
overload string\_length with string0\_length of 0
overload string\_length with string1\_length of 10
```

#### example

```
// strlen.dats

#include "share/atspre_staload.hats"

implement main0() = {
  val a = "add"
  val () = println!("'", a, "' length = ", strlen(a))
  val () = println!("'", a, "' length = ", string_length(a))
}
```

#### output

```
'add' length = 3
'add' length = 3
```

## string_nlength

```
fun{
} string0\_nlength
  (x: NSH(string), n: size\_t):<> size\_t
fun{
} string1\_nlength
  {n1,n2:int}
  (NSH(string(n1)), size\_t(n2)):<> size\_t(min(n1,n2))
overload string\_nlength with string0\_nlength of 0
overload string\_nlength with string1\_nlength of 10
```

#### example

```
```

#### output

```
```

## 

```
fun{
} string0\_copy
  (xs: NSH(string)):<!wrt> Strptr1
```

#### example

```
```

#### output

```
```

## 

```
fun{
} string1_copy
  {n:int} (xs: NSH(string(n))):<!wrt> strnptr (n)
```

#### example

```
```

#### output

```
```

## 

```
symintr string\_append
```

#### example

```
```

#### output

```
```

## 

```
fun{
} string0\_append
(
  x1: NSH(string), x2: NSH(string)
) :<!wrt> Strptr1 // end-of-fun
```

#### example

```
```

#### output

```
```

## 

```
fun{
} string1\_append
  {n1,n2:int} (
  x1: NSH(string(n1)), x2: NSH(string(n2))
) :<!wrt> strnptr (n1+n2) // end of [string1\_append]
```

#### example

```
```

#### output

```
```

## 

```
overload string\_append with string0\_append of 0
```

#### example

```
```

#### output

```
```

## 

```
(*
overload string\_append with string1\_append of 20
*)
```

#### example

```
```

#### output

```
```

## 

```
symintr string\_append3
```

#### example

```
```

#### output

```
```

## 

```
fun{
} string0\_append3
(
  x1: NSH(string), x2: NSH(string), x3: NSH(string)
) :<!wrt> Strptr1 // end-of-fun
```

#### example

```
```

#### output

```
```

## 

```
overload string\_append3 with string0\_append3 of 0
```

#### example

```
```

#### output

```
```

## 

fun{
} stringarr\_concat{n:int}
  (xs: arrayref(string, n), size\_t n):<!wrt> Strptr1

#### example

```
```

#### output

```
```

## 

```
fun{
} stringlst_concat (xs: List(string)):<!wrt> Strptr1
```

#### example

```
```

#### output

```
```

## string_explode

```
fun{
} string_explode
  {n:int} (x: string(n)):<!wrt> list_vt (charNZ, n)
```

#### example

```
```

#### output

```
```

## 

```
fun{
} string_tabulate$fopr (size_t): charNZ
```

#### example

```
```

#### output

```
```

## 

```
fun{
} string_tabulate{n:int} (n: size_t(n)): strnptr(n)
```

#### example

```
```

#### output

```
```

## string_foreach$cont

```
fun{env:vt0p}
string_foreach$cont (c: char, env: &env): bool
```

#### example

```
```

#### output

```
```

## string\_foreach$fwork

```
fun{env:vt0p}
string_foreach$fwork (c: char, env: &(env) >> _): void
```

#### example

```
```

#### output

```
```

## string\_foreach

```
fun{
} string_foreach {n:int} (str: string(n)): sizeLte(n)
```

#### example

```
```

#### output

```
```

## string\_foreach\_env

```
fun{
env:vt0p
} string_foreach_env
  {n:int} (str: string(n), env: &(env) >> _): sizeLte(n)
```

#### example

```
```

#### output

```
```

## string\_rforeach$cont

```
fun{env:vt0p}
string_rforeach$cont (c: char, env: &env): bool
```

#### example

```
```

#### output

```
```

## string\_rforeach$fwork

```
fun{env:vt0p}
string_rforeach$fwork (c: char, env: &(env) >> _): void
```

#### example

```
```

#### output

```
```

## 

```
fun{
} string_rforeach {n:int} (str: string(n)): sizeLte(n)
```

#### example

```
```

#### output

```
```

## string\_rforeach\_env

```
fun{
env:vt0p
} string_rforeach_env
  {n:int} (str: string(n), env: &(env) >> _): sizeLte(n)
```

#### example

```
```

#### output

```
```

## 

```
(*
** HX: [stropt_none] is just the null pointer
*)
fun stropt_none (): stropt (~1) = "mac#%"
```

#### example

```
```

#### output

```
```

## stropt_some

```
symintr stropt_some
```

#### example

```
```

#### output

```
```

## stropt0\_some

```
castfn stropt0_some (x: SHR(string)): Stropt1
```

#### example

```
```

#### output

```
```

## 

overload stropt\_some with stropt0\_some of 0

#### example

```
```

#### output

```
```

## 

castfn stropt1\_some {n:int} (x: SHR(string n)): stropt (n)

#### example

```
```

#### output

```
```

## 

overload stropt\_some with stropt1\_some of 10

#### example

```
```

#### output

```
```

## 

fun{
} stropt\_is\_none{n:int} (stropt(n)):<> bool (n < 0)

#### example

```
```

#### output

```
```

## 

fun{
} stropt\_is\_some{n:int} (stropt(n)):<> bool (n >= 0)

#### example

```
```

#### output

```
```

## 

castfn
stropt\_unsome {n:nat} (x: stropt n):<> string (n)

#### example

```
```

#### output

```
```

## 

fun{
} stropt\_length
  {n:int} (x: stropt (n)):<> ssize\_t (n)

#### example

```
```

#### output

```
```

## 

fun print\_stropt (opt: Stropt0): void = "mac#%"

#### example

```
```

#### output

```
```

## 

fun prerr\_stropt (opt: Stropt0): void = "mac#%"

#### example

```
```

#### output

```
```

## 

fun fprint\_stropt (out: FILEref, opt: Stropt0): void = "mac#%"

#### example

```
```

#### output

```
```

## 

// overloading for certain symbols

#### example

```
```

#### output

```
```

## 

overload
[] with string\_get\_at\_size of 1

#### example

```
```

#### output

```
```

## 

overload
[] with string\_get\_at\_gint of 0

#### example

```
```

#### output

```
```

## 

overload
[] with string\_get\_at\_guint of 0

#### example

```
```

#### output

```
```

## 

overload iseqz with string\_is\_empty

#### example

```
```

#### output

```
```

## 

overload isneqz with string\_isnot\_empty

#### example

```
```

#### output

```
```

## 

overload .head with string\_head

#### example

```
```

#### output

```
```

## 

overload .tail with string\_tail

#### example

```
```

#### output

```
```

## 

overload length with string\_length

#### example

```
```

#### output

```
```

## 

overload copy with string0\_copy of 0

#### example

```
```

#### output

```
```

## 

(*
//
// HX: too much of a surprise!
//
overload copy with string1\_copy of 10
*)

#### example

```
```

#### output

```
```

## 

overload print with print\_string of 0

#### example

```
```

#### output

```
```

## 

overload prerr with prerr\_string of 0

#### example

```
```

#### output

```
```

## 

overload fprint with fprint\_string of 0

#### example

```
```

#### output

```
```

## 

overload iseqz with stropt\_is\_none

#### example

```
```

#### output

```
```

## 

overload isneqz with stropt\_is\_some

#### example

```
```

#### output

```
```

## 

overload length with stropt\_length

#### example

```
```

#### output

```
```

## 

overload print with print\_stropt of 0

#### example

```
```

#### output

```
```

## 

overload prerr with prerr\_stropt of 0

#### example

```
```

#### output

```
```

## 

overload fprint with fprint\_stropt of 0
