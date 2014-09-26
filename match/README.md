# Scalaのunapplyでパターンマッチとそのrubyでの動作

## a.scala

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


## F#のアクティブパターン

http://fsharpintro.net/activePattern.html

## a.ruby

```
class A
  attr :a
  def initialize(a)
    @a = a
  end
  def self.unapply(a)
    print("unapply\n")
    if (a.a > 0) then
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

```
ruby a.rb
unapply
unapply
b=1
b2=-1
```

### 参考

http://www.callcc.net/diary/20120204.html

## fsharp

http://fsharpintro.net/activePattern.html

## 

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
