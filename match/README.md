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

## ブロック文法の案

現状は動作しません。

```
#b = if (a=A.unapply(a1)) != nil then a[0] else -a1.a end

b = a1{
  |A(a)| a;
  |a| -a.a
}

```

## caseの拡張案

とりあえず書いたのだけど、when thenのthenは省略可能なことを考慮に入れる必要がある。

```
def e(a)
  case a
  when |A(a)| if a > 0 then e(a)
  when |a| then a
  end
end
```

省略が
```
def e
  when |a| then a
end
```

## パターンマッチのコンパイルの最適化についての論文

表示的意味論に基づくパターンマッチング
コンパイル方式の構築と実装 堀江淳, Satoshi OSAKA

https://www.jstage.jst.go.jp/article/jssst/24/2/24_2_2_113/_pdf


