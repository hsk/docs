# スタートATS

  簡単なATSの入門ページ

  インストールは出来てる事前提で、色々な機能を普通に使ってみる話。


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

## if

    // if.dats

    #include "share/atspre_staload.hats"

    fn f(v:int):int = 
      if v = 10 then 1 else 2

    implement main0() = {
      val v:int = 10

      val v2:int = f(v)
      val () = println!("v2=",v2)
    }

## 多相性 poly

  テンプレートを使ってみましょう。
  `+`演算子等は`int`や`float`等の型にしかないため、多相的にするには`gadd_val`関数等を使います。

    // poly.dats

    #include "share/atspre_staload.hats"

    fun{a:t@ype} add(f: a, g: a) :a = gadd_val(f, g)
    fun{a:t@ype} sub(f: a, g: a) :a = gsub_val(f, g)
    fun{a:t@ype} mul(f: a, g: a) :a = gmul_val(f, g)
    fun{a:t@ype} div(f: a, g: a) :a = gdiv_val(f, g)
    fun{a:t@ype} mod(f: a, g: a) :a = gmod_val(f, g)

    implement main0() = {
      val () = println!("add(1,2)=",add<int>(1,2));
      val () = println!("sub(1,2)=",sub<int>(1,2));
      val () = println!("mul(1,2)=",mul<int>(1,2));
      val () = println!("div(1,2)=",div<int>(1,2));
      val () = println!("mod(1,2)=",mod<int>(1,2));
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