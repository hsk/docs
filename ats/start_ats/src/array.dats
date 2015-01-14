// array.dats

#include "share/atspre_staload.hats"

fun f1(A: &array(int, 10)): int = sum where {
  var i:[a:int | a >= 0 && a <= 10]int(a)
  var sum:int = 0
  val () = for (i := 0; i < 10; i := i+1) sum := sum + A[i]
}

fun f2{n:nat}(A: &array(int, n),len:int(n)): int = sum where {
  var i:[a:int | a >= 0 && a <= n]int(a)
  var sum:int = 0
  val () = for (i := 0; i < len; i := i+1) sum := sum + A[i]
}


implement main0 () = {

  var A = @[int][10]() // A: array(int?, 10) // 初期化されない配列
  var A = @[int][10](0) // A: array(int, 10) // 0で初期化
  var A:array(int,10) = @[int](0, 1, 2, 3, 4, 5, 6, 7, 8, 9) // A: array(int, 10)

  val () = println!(A[1])
  val () = println!(f1(A))
  val () = println!(f2(A,10))

}

