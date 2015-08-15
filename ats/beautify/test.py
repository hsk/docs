from beauty import *

print(parse("()"))

print(parse("1"))

print(parse("begin\n  1 + 2 \nend"))

print(parse(
    "if a then b else c\n" +
    ""))
print(parse(
    "let\n" +
    " val a = 1\n" +
    "in\n" +
    "if a then b else c\n" +
    "end"))

print(parse(
    "let val a = 1 in\n" +
    "if a\n" +
    "then b\n" +
    "else c\n" +
    "end\n"))

print(parse(
    "let val a = 1 in\n" +
    "if a\n" +
    "then\n" +
    "b\n" +
    "else\n" +
    "c\n" +
    "end\n"))

print(parse("""
    let val a =
        let val a = 1 in
        a end
    in
    a
    end"""))

print(parse(
    "if a > 10 then\n" +
    "b\n" +
    "else if a > 20 then\n" +
    "c\n" +
    "else\n" +
    "d\n" +
    ""))

print(parse(
    "if a > 10 then b else\n" +
    "if a > 20 then c else\n" +
    "d\n" +
    ""))

print(parse(
    """
    implement main0 () =
    let
    val a = 1
    val b = 2
    in
    a + b
    end
    """))

print(parse(
    """
    implement main0 () =
    {
    val a = 1
    val b = 2
    }
    """))

print(parse(
    """
    implement main0 () =
    {
    val a = 1
    val b = 2
    val () = println!("hello")
    }
    """))

print(parse(
    """
    implement main0 () = {
    val a = 1
    val b = 2
    val () = println!("hello")
    }
    """))

print(parse(
    """
    #include "share/atspre_define.hats"
    #include "share/atspre_staload.hats"

    val a = 1
    val b = ~2
    val c = 1+2
    val d = 1+2*3-4
    val e = (1+2)/(3-4)
    val
    PI = 3.14
    and
    radius = 10.0


val area = let
  val PI = 3.14 and radius = 10.0 in PI * radius * radius
end

val area =
  PI * radius * radius where {
  val PI = 3.14 and radius = 10.0
}

    """))

print(parse(
    """
    local
    val PI = 3.14 and radius = 10.0
    in
    val area = PI * radius * radius
    end
    """))


print(parse(
    """

    val
    xyz = ('A', 1, 2.0)
     val x = xyz.0 and y = xyz.1 and z = xyz.2
      val xyz = ('A', 1, 2.0)
       val (x, y, z) = xyz // x = 'A'; y = 1; z = 2.0
        val
            xyz = '(
            'A', 1, 2.0
        )
         val
         x = xyz.0
         and y = xyz.1 and z = xyz.2
    """))

print(parse(
    """
typedef
point2D =
@{ x= double }
    val
    xyz = 1
    """))

print(parse(
    """
typedef
point2D =
@{ x= double,y = double }
    val
    xyz = 1
    """))

print(parse(
    """

typedef
point2D =
@{ x= double, y= double }
    val
    xyz = ('A', 1, 2.0)
val theOrigin =
@{ x= 0.0, y= 0.0 } : point2D
val theOrigin_x =
theOrigin.x and theOrigin_y = theOrigin.y
val @{ x= theOrigin_x, y= theOrigin_y } =
theOrigin
val @{ x= theOrigin_x, y= theOrigin_y } = theOrigin
val @{ y= theOrigin_y, y= theOrigin_y } = theOrigin
    """))

print(parse(
    """
    (print 'H';
        print 'e';
            print 'l';
                print 'l';
                    print 'o')

let
  val () = print 'H'
  val () = print 'e'
  val () = print 'l'
  val () = print 'l'
  val () = print 'o'
in
end

    """))

# TODO
print(parse(
    """
// one line comment

/*
 are
*/

(* ml style *)
    (print 'H';
        print 'e';
            print 'l';
                print 'l';
                    print 'o')

let
  val () = print 'H'
  val () = print 'e'
  val () = print 'l'
  val () = print 'l'
  val () = print 'o'
in
end

////

restof
    """))

print(parse(
    """
        fn square (x: double): double =
        x * x
        val square =
        lam (x) =>
        x * x
        val square =
        lam (y: double): double =>
        y * y
    """))

# http://ats-lang.sourceforge.net/DOCUMENT/INT2PROGINATS/HTML/x460.html

print(parse(
    """
fn sqrsum1 (x: int, y: int): int = x * x + y * y
//
typedef int2 = (int, int)
//
fn sqrsum2
  (xy: int2): int =
  let val x = xy.0 and y = xy.1 in x * x + y * y end
// end of [sqrsum2]
    """))

print(parse(
    """
fun succ_int (x: int): int // successor
    fun pred_int (x: int): int // predecessor

        fun add_int_int (x: int, y: int): int // +

fn abs (x: double): double =
if x >= 0.0
then x
else ~x


fun sum3
  (m: int, n: int): int =
  if m <= n then let
    val mn2 = (m+n)/2 in sum3 (m, mn2-1) + mn2 + sum3 (mn2+1, n)
  end else 0 // end of [if]
// end of [sum3]
    """))

# todo one line comment
# http://ats-lang.sourceforge.net/DOCUMENT/INT2PROGINATS/HTML/x593.html
print(parse(
    """
typedef int4 = (int, int, int, int)

val theCoins = (1, 5, 10, 25): int4

fun coin_get
  (n: int): int =
  if n = 0 then theCoins.0
  else if n = 1 then theCoins.1
  else if n = 2 then theCoins.2
  else if n = 3 then theCoins.3
  else ~1 (* erroneous value *)
// end of [coin_get]

fun coin_change
  (sum: int): int = let
  fun aux (sum: int, n: int): int =
    if sum > 0 then
     (if n >= 0 then aux (sum, n-1) + aux (sum-coin_get(n), n) else 0)
    else (if sum < 0 then 0 else 1)
in
  aux (sum, 3)
end
    """))

print(parse(
    r"""
case x of
    a => b

fun coin_get
  (n: int): int = a

datatype a =
  | a
  | a of ()
  | bb{a:int} of ()

case x of
 | a => b

fun coin_get
  (n: int): int = a

case x of
 | a => b
 | a => b

case x of
    a => b
 | a => b

fun coin_get
  (n: int): int = a

    """))

print(parse(
    r"""
case x of
    a => b

fun coin_get
  (n: int): int = a

datatype a =
  | a
  | a of ()
  | bb{a:int} of ()

case x of
 | a => b

fun coin_get
  (n: int): int = a

case x of
 | a => b
 | a => b

case x of
    a => b
 | a => b

fun coin_get
  (n: int): int = a

    """))

print(r"""|""")

print(parse(
    r'''
fun aux (t: tree a): int =
(
if df = 0 orelse df = 1
then 1+szl+szr
else $raise Negative()
)

    '''))

start = "ATS-Postiats/doc/BOOK/INT2PROGINATS/CODE/"
data = open(start+"INT2PROGINATS-CODE.php").read()


r = re.compile(r"listitem_gen\('(.*?)', '(.*?)'")
lst = r.findall(data)
cnt = 0

if True:
    for i in lst:
        cnt = cnt + 1
        f = i[0] + "/" + i[1]
        print("*****************************************")
        print("test " + str(cnt) + " : " + f)
        print("*****************************************")
        print(parse(open(start+f).read()))

if False:
    tests = [
        #"variant", "when", "list"
    ]

    for f in tests:
        path = "../test/" + f + ".ml"
        print("test " + path)
        print(parse(open(path).read()))

    print(parse(
        "type e=\n" +
        "   | Add of int\n" +
        "   | Mul of int\n" +
        "   ;;  \n" +
        "   match a with\n" +
        "       | Add(a) -> a\n" +
        "       | Mul(a) -> a\n" +
        ""))
    print(parse(
        "type e=\n" +
        "   Add of int\n" +
        "   ;;  \n" +
        "   1\n" +
        ""))


    print(parse(
        "  open a;;\n" +
        "  let a = 1\n" +
        ""))


    print(parse(
        '''
        try
        a;
        with
        e
        '''))

    print(parse(
        r'''
        open Printf
        let a = 1
        let b = 2
        let _ =
            let c = a + b in
            printf("%d\n", c)
        '''))

    print(parse(
        r'''
        module a = struct
            let a = 1
            let b = 2
        end
        module a =
            Set.Make(String)
        module a =
            Set.Make(struct type t = s end)
        module F (X : X_type) = struct

        end
        '''))

print(parse(
    r'''
fun aux (t: tree a): int =
(
if df = 0 orelse df = 1
then 1+szl+szr
else $raise Negative()
)

    '''))
