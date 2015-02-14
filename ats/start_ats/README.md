# スタートATS

  簡単なATSの入門ページ

  主なATSの機能を使ってみます。

## Hello World!

    // hello.dats
    implement main0() = {
    	val () = println!("Hello World!")
    }

  こんなファイルを作っておきます。

    $ patscc hello.dats 
    $ ./a.out
    Hello World!

  a.outを消しておきましょう。

    $ rm a.out

## Makefile

  Makefileを作ってビルドを自動化しましょう。

    hello: hello.dats
    	patscc -o hello hello.dats

    clean:
    	rm *_dats.c hello

  make helloと入力する事でビルド出来て、./helloで実行出来ます。

    $ make hello
    $ ./hello
    Hello World!

  make cleanで不要なファイルを消しましょう。

    $ make clean

## 整数

  intを使ってみましょう。

    // int.dats

    implement main0() = {
    	val v:int = 10
    	val () = println!(v)
    }

  Makefileに以下のように書き加えます。

    int: int.dats
    	patscc -o int int.dats

    clean:
    	rm *_dats.c hello int

  コンパイル&実行してみましょう。

    $ make int
    $ ./int
    10

  10と表示されました。

## 型推論

  ATSは型推論があるので:intを消しても動きます。

    // int2.dats

    implement main0() = {
    	val v = 10
    	val () = println!(v)
    }

  Makefileはいい感じに書き加えて、実行してみましょう。

    $ make int2
    $ ./int2
    10

  しかし、ATSの型推論は関数の型を推論してくれないので注意が必要です。

## 関数 fn

  関数を使ってみましょう。

    // fn.dats

    #include "share/atspre_staload.hats"

    fn add(a:int,b:int):int = begin
      a + b
    end

    implement main0() = {
      val () = println!("add(1,2)=",add(1,2))
    }

## 再帰関数 fun

  再帰関数にするにはfunを使って関数を定義します。

    // fun.dats

    #include "share/atspre_staload.hats"

    fun sum(n:int):int = begin
      if n = 0
      then 0
      else n+sum(n-1)
    end

    implement main0() = {
      val () = println!("sum(10)=",sum(10))
    }

## let

  SMLのlet in endがあります。

    // let.dats

    #include "share/atspre_staload.hats"

    fn dt():int = let
      val a = 1
      val b = 2
      val c = a + b
      val d = 3
    in
      c + d
    end

    implement main0() = let
      val v = dt()
      val () = println!("v=",v)
      val () = println!("n=",let val n = 1 in n end)
    in
      ()
    end

## where

  ATSのwhereはHaskellのwhereっぽく後ろに書く事が出来ますが、上から順番に実行され、遅延評価されるような事はありません。以下の例では、"kore" "kore2" も表示されます。

    // where.dats

    #include "share/atspre_staload.hats"

    fn f():int = b where {
      val a = 1
      val b = 2
      val () = println!("kore")
      val () = println!("kore2")
      val c = a + b
      val d = c
    }

    implement main0() = {
      val v = f()
      val () = println!("v=",v)
    }

## if then else

  ATSではifは式です。

    // if.dats

    #include "share/atspre_staload.hats"

    fn f(v:int):int = 
      if v = 10 then 1 else 2

    implement main0() = {
      val v:int = 10

      val v2:int = f(v)
      val () = println!("v2=",v2)
    }

## case

  case ofでパターンマッチ出来ます。

    // case.dats

    #include "share/atspre_staload.hats"

    fn f1(v:int):int = case v of 1 => 2 | _ => 3

    fn f2(v:int):int =
      case v of 
      | 1 => 2
      | _ => 3

    fn f3(v:int):int =
      begin case v of 
        | 1 => 2
        | _ => 3
      end

    implement main0() = begin
      println!(f1(3));
      println!(f2(3));
      println!(f3(3));
      ()
    end

## var スタック上の変数

  varを使ってスタック上の変数を使うと変数を書き換える事が出来ます。

    // var.dats

    implement main0 () = let
      var v: int = 1
      val () = v := 33
    in
      println!(v)
    end

## whileループ

  whileループもあります。

    // while.dats

    #include "share/atspre_staload.hats"

    fun fact (n: int): int =
      if n > 0 then n * fact(n-1) else 1

    fun fact2(n: int): int = let
      var n: int = n
      var res: int = 1
    in
      while (n > 0) (res := res * n; n := n - 1); res
    end

    implement main0 () = {
      val () = println!(fact(10))
      val () = println!(fact2(10))
    }

## forループ

  forループもあります。

    // for.dats

    #include "share/atspre_staload.hats"

    fun fact (n: int): int =
      if n > 0 then n * fact(n-1) else 1

    fun fact3
      (n: int): int = let
      var i: int
      var res: int = 1
    in
      for (i := n; i > 0; i := i-1) res := res * i; res
    end

    implement main0 () = {
      val () = println!(fact(10))
      val () = println!(fact3(10))
    }

## record

  構造体

    // record.dats

    #include "share/atspre_staload.hats"

    typedef point2D = @{ x= double, y= double }

    fn f1(a:point2D):double = begin
      a.x + a.y
    end

    fn f2(a:point2D):double = begin
      begin case a of
        | @{x=a,y=b} => a + b
      end
    end

    implement main0() = {
      val e = @{x=1.1,y=2.2}
      val () = println!(e.x)
      val () = println!(f1(e))
    }

## tuple

  タプル、多値

    // tuple.dats

    #include "share/atspre_staload.hats"

    fn f1(a:(int,int)):int = a.0
    fn f2(a:(int,int)):int = let
      val (x,y) = a
    in
      x
    end

    fn f3(a:(int,int)):int = let
      val x = case a of _ => 0
    in
      x
    end

    implement main0() = {
      val e = (1,2)
      val () = println!(e.0)
      val () = println!(e.1)
      val () = println!(f1(e))
      val () = println!(f2(e))
      val () = println!(f3(e))
    }

## datatype

  datatypeで代数データ型を使う事が出来ます。パラメータがない場合は of ()と書く点が変わっている所です。

    // datatype.dats

    #include "share/atspre_staload.hats"

    datatype e =
      | ENil of ()
      | EInt of (int)
      | EAdd of (e, e)
      | ESub of (e, e)

    fun eval(e:e):int =
      begin case e of
        | ENil()  => 0
        | EInt(i) => i
        | EAdd(a, b) => eval(a) + eval(b)
        | ESub(a, b) => eval(a) + eval(b)
      end

    implement main0() = {
      val () = println!(eval(ESub(EAdd(EInt(1),EInt(2)),EInt(1))))
    }

## array 配列と依存型

  ATSの配列をforループで計算する場合、単純にループカウンタのiをint型にするとエラーになってしまいます。
  それでは、配列の外側にアクセスしてしまう可能性があるからです。
  そこで、カウンタiの型を依存型を使って範囲を限定することで正しく動作するように出来ます。
  配列の長さも渡すようにする場合{n:nat}を使って配列の長さと関連づける事で一般的にすることも可能です。

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


## 多相性 poly

  C++でいうところのテンプレートを使ってみましょう。
  `+`演算子等は`int`や`float`等の型にしかないため、多相的にするには`gadd_val`関数等を使います。

    // poly.dats

    #include "share/atspre_staload.hats"

    fun{a:t@ype} id(x:a) :a = x

    fun{a:t@ype} add(f: a, g: a) :a = gadd_val(f, g)
    fun{a:t@ype} sub(f: a, g: a) :a = gsub_val(f, g)
    fun{a:t@ype} mul(f: a, g: a) :a = gmul_val(f, g)
    fun{a:t@ype} div(f: a, g: a) :a = gdiv_val(f, g)
    fun{a:t@ype} mod(f: a, g: a) :a = gmod_val(f, g)

    implement main0() = {
      val () = println!("id(1)=",id<int>(1));
      val () = println!("add(1,2)=",add<int>(1,2));
      val () = println!("sub(1,2)=",sub<int>(1,2));
      val () = println!("mul(1,2)=",mul<int>(1,2));
      val () = println!("div(1,2)=",div<int>(1,2));
      val () = println!("mod(1,2)=",mod<int>(1,2));
    }

## ATSからC言語のコードの呼び出し


  参考

  https://github.com/metasepi/linux-bohai-s1/blob/bohai-s1/metasepi/net/sunrpc/DATS/xdr.dats#L25

  https://github.com/metasepi/linux-bohai-s1/blob/bohai-s1/metasepi/include/linux/sunrpc/SATS/xdr.sats#L19


## C言語からATSの呼び出し

  ATSのコードを書きます。

    // atsfunc.dats
    #define ATS_DYNLOADFLAG 0
    #include "share/atspre_staload.hats"

    extern fun fib (n: uint): uint = "ext#ats_fib"
    implement fib (n: uint): uint =
      case+ n of
      | 0U => 0U
      | 1U => 1U
      | _  => fib(n - 2U) + fib(n - 1U)


  C言語のコードを書きます。

    // main.c
    #include <stdio.h>

    extern unsigned int ats_fib(unsigned int);

    int main() {
      printf("fib(7) = %u\n", ats_fib(7));
      return 0;
    }

  atsfuncをコンパイルします。

    $ patscc -c atsfunc.dats

  cのプログラムとatsをリンクします。

    $ gcc -I${PATSHOME} -I${PATSHOME}/ccomp/runtime -o main main.c atsfunc_dats.c

  実行します。

    $ ./main
    fib(7) = 13

  - 参考
    - https://github.com/jats-ug/practice-ats/tree/master/call-ats-fromc

## 証明する

    dataprop FIB (int, int) =
      | FIB0 (0, 0) of ()
      | FIB1 (1, 1) of ()
      | {n:nat}{r0,r1:nat}
        FIB2 (n+2, r0+r1) of (FIB (n, r0), FIB (n+1, r1))
  
  これは、一つ目の引数が引数の値で、二つ目の引数が答えであるような関数としてFIBをみると
  こう書けます。

  fib(0) = 0, and
  fib(1) = 1, and
  fib(n+2) = fib(n+1) + fib(n) for n >= 2. 

## 線形型を使ってみる


  線形型を使うとリソースの持ち主を決めて、持ち回り誰かが最後に消費する必要があります。

    // patscc -DATS_MEMALLOC_LIBC calc_cpy.dats -o calc_cpy

    #include "share/atspre_staload.hats"

    datavtype e =
      | EInt of (int)

    // データを借りてコピーする
    fun e_copy(e: !e): e =
      begin case+ e of
        | EInt(x) => EInt(x)
      end

    // データを受け取って消費する
    fun e_free(e: e): void =
      begin case+ e of
        | ~EInt(x) => ()
      end

    // データを借りて計算する
    fun e_eval(e: !e): int =
      begin case+ e of
        | EInt(x) => x
      end

    extern fun e_print(!e): void
    overload print with e_print

    // データを借りて来て出力する
    implement e_print(e) =
      begin case+ e of
        | EInt(x) => print!("EInt(",x,")")
      end

    implement main0() = {

      val e0 = EInt 1
      val e2 = e_copy e0

      val () = println!e2
      val () = println!(e_eval e2)

      val () = e_free e0
      val () = e_free e2
    }


