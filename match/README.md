# Scalaのunapplyでパターンマッチとそのrubyでの動作

## a.scala

ScalaのExtractor Patternsがunapplyメソッドを使ってパターンマッチを自由に拡張出来ます。


```
package mat

object Main extends App {
  class A(val a:Int)
  object A {
    def unapply(a:A):Option[Int] = {
      println("unapply")
      if (a.a > 0) Some(a.a) else None
    }
  }
  val b = new A(1) match {
    case A(a) => a
    case k:A => -k.a
  }
  val b2 = new A(1) match {
    case A(a) => a
    case k:A => -k.a
  }
  println("b="+b)
  println("b2="+b2)
}
```

実行例

```
$ scalac a.scala
$ scala mat.Main
unapply
unapply
b=1
b2=1
```

### 参考

パターンマッチをもっと便利に - extractor(抽出子)による拡張

http://yuroyoro.hatenablog.com/entry/20100709/1278657400

Chapter 8 Pattern Matching The Scala Language Specification Version 2.9


http://www.scala-lang.org/files/archive/nightly/pdfs/ScalaReference.pdf

## F#のアクティブパターン

http://fsharpintro.net/activePattern.html

## a.ruby

RubyでScalaのunapplyを書いてみたもの。

```
class A
  attr :a
  def initialize(a)
    @a = a
  end
  def self.unapply(a)
    print("unapply\n")
    if a.kind_of A && (a.a > 0) then
      [a.a]
    else
      nil
    end
  end
end

a1 = A.new(1)
a2 = A.new(-1)
b =
  if (a=A.unapply(a1)) != nil
  then (lambda{|a| a}).call(a[0])
  else (lambda{|k| -k.a}).call(a1)
  end
b2 =
  if (a=A.unapply(a2)) != nil
  then (lambda{|a| a}).call(a[0])
  else (lambda{|k| -k.a}).call(a1)
  end

printf("b=%d\n",b)
printf("b2=%d\n",b2)
```

実行例

```
ruby a.rb
unapply
unapply
b=1
b2=-1
```

Scalaのunapplyの問題点は、ifの連続になるので、タグによるテーブルジャンプが行われない為に
高速化が難しそうな点です。

### 参考

ライブラリ化した例

http://www.callcc.net/diary/20120204.html

## fsharp

F#のアクティブパターンはまた別のアプローチです。

http://fsharpintro.net/activePattern.html

```
let (|ODD|EVEN|) (x:int) =
	if x%2=0 then EVEN else ODD;;
let (|POSITIVE|NEGATIVE|) (x:int) =
	if x>=0 then POSITIVE else NEGATIVE;;

match 10 with
	| POSITIVE -> print_endline "positive"
	| NEGATIVE -> print_endline "negative";;

match 10 with
	| ODD -> print_endline "odd"
	| EVEN -> print_endline "even";;
```

パターンマッチ出来る内容で関数を作成して受け取った値で値が帰ります。
この例ではパターンマッチの変数バインディングがないのですが、
バインディングも可能です。
この実装を見る限りでは、アクティブパターンでは、１つの処理で分岐を記述出来るため、
Scalaのunapplyよりも高速に動作させる事が期待出来そうです。


http://igeta.cocolog-nifty.com/blog/2008/04/active_patterns.html

```
module WithinTwoMinutes =
struct
  // x / y が割り切れるときにその商を返すアクティブ パターン
  let (|Mul|_|) x y = if y % x = 0 then Some (y / x)
                                   else None
  
  // 与えられたリストを fizzbuzz する
  let fizzbuzz (s:#seq<int>) =
    // 1 コの数値に対する fizzbuzz 判定
    let fizzbuzz' = function
      | Mul 15 _ -> "FizzBuzz"
      | Mul  3 _ -> "Fizz"
      | Mul  5 _ -> "Buzz"
      | n -> string_of_int n
    in
    Seq.map fizzbuzz' s
end;;

module Program =
struct
  open WithinTwoMinutes
  
  // 1 ～ 100 まで fizzbuzz する
  let main () =
    fizzbuzz {1..100}
    |> Seq.iter (printf "%s ");
    print_string "\n"
end;;

let _ = Program.main ();;
```

このように、1つの関数で全パターンを

## ブロック文法の案

現状は動作しません。

```
#b = if (a=A.unapply(a1)) != nil then a[0] else -a1.a end

b = a1{
  |A(a)| a;
  |a| -a.a
}

```

このようなブロックの拡張を考えた理由は、OCamlのfunctionと
Scalaのパーシャルファンクションのパターンマッチ等にも対応させたいと言う理由です。

Scala

```
def m(a:A)(f:A=>Int):Int = {
  f(a)
}
b = m(a){
  case A(a) if a > 0 => a
  case a => -a.a
}
```

OCaml

```
let m(a:A)(f:a' -> int):int =
  f a

b = m(a) begin function
  | A(a) when a > 0 -> a
  | A(a) -> -a
end
```
Ruby

```
def m(a,f)
  f.call(a)
end

b = m(a) {
  |A(a)| when a > 0 then a;
  |a| -a.a
}
```

;を区切りと考えた理由はErlangがそのような実装だった為です。

```
case Temperature of
{celsius, N} when N >= 20, N =< 45 ->
  'favorable';
{kelvin, N} when N >= 293, N =< 318 ->
  'scientifically favorable';
{fahrenheit, N} when N >= 68, N =< 113 ->
  'favorable in the US';
_ ->
  'avoid beach'
```

## caseの拡張案

とりあえず書いたのだけど、when thenのthenは省略可能なことを考慮に入れる必要があります。

Ruby案

```
def e(a)
  case a
  when |A(a)| if a > 0 then e(a)
  when |a| then a
  end
end
```

省略のRuby案

```
def e
  when |a| then a
end
```

この省略方法はOCamlのfunctionを使った省略を参考にすると良いかと考えます。

```
let e a =
  match a with
  | A(a) when a > 0 -> a
  | A(a) -> -a 
```
を省略すると
```
let e = function
  | A(a) when a > 0 -> a
  | A(a) -> -a
```

Scalaでは
```
def e(a:A):Int = {
  a match {
  | A(a) if a > 0 => a
  | a => -a.a
  }
}
```

を

```
val e:(A=>Int) = {
  case A(a) if a > 0 => a
  case a => -a.a
}
```

と省略可能です。Scalaのvalはダサい感じがするので、
defで省略出来れば嬉しいわけです。

## defの連続によるパターンマッチ

Ruby案

```
def e(|A(a)|when a >0) a end
def e(|a|) -a.a end
```

defの連続によるパターンマッチは出来るのであれば欲しい所ですが、Haskellのソースを
ぱっと見てメソッドだらけになると混乱する可能性があるように思うので、難しい所です。


## パターンマッチのコンパイルの最適化についての論文

表示的意味論に基づくパターンマッチング
コンパイル方式の構築と実装 堀江淳, Satoshi OSAKA

https://www.jstage.jst.go.jp/article/jssst/24/2/24_2_2_113/_pdf

