# newml

新しいML言語

newmlはOCamlへのトランスレータ言語です。

Scalaライクな文法をOCamlに導入します。
Scalaには似ていますが、OCamlに合わせてカスタマイズしており、
C言語風に記述出来てより使いやすくする事を目標としています。

## インストール

OCamlとOMakeがある環境で、

    $ omake

と入力します。

## Hello World

```
open Printf
printf("hello world!\n")
```
hello.nml

```
$ newmlc hello.nml hello.ml
$ ocaml hello.ml
hello world!
```

## 特徴

カリー化された関数の記述がとても楽です。

以下のように関数f5を定義して、呼び出す事が出来ます:

```
f5 := {
 a b c d e => a + b + c + d + e
}

printf("%d\n" f5(1 2 3 4 5))
```

この関数をOCamlで書くと以下のようになります:
```
let f5 = (
  fun a b c d e -> a + b + c + d + e
)
printf "%d\n" (f5 1 2 3 4 5)
```

この関数はScalaで書くと以下のような意味になります。

```
val f5 = { (a:Int) => (b:Int) => (c:Int) => (d:Int) => (e:Int) =>
  a + b + c + d + e
}
printf("%d\n")(f5(1)(2)(3)(4)(5))
```

Scalaのprintfはカリー化されていませんが、OCamlのprintfはカリー化されているのでこのように書いています。
また、Scalaはdefで書けばもっと短くかけます。
実際にScalaで動かす場合は以下のように書きます:

```
def f5(a:Int)(b:Int)(c:Int)(d:Int)(e:Int) = a + b + c + d + e
printf("%d\n",f5(1)(2)(3)(4)(5))
```

関数の引数はカンマ区切りで記述するとタプルとなります。
そのため、複数の式を記述する事が出来るようにして、複数あった場合はカリー化された関数の呼び出しとする事で
実現しています。

タプルの関数

```
f := {
  a,b => a + b
}
printf("%d\n")(f(1,2))
printf("%d\n" f(1,2))
```

タプルを２つ受け取る関数

```
f2 := {
  a,b c,d => a*b + c*d
}
printf("%d\n")(f2(1,2)(3,4))
printf("%d\n" f2(1,2 3,4))
```

## 実装している機能

- 変数
- if else 式
- match 式
- パーシャルファンクション
- レコード
- 代数データ型
- ブロック

## サンプル

[examples/test.nml](test.nml)とその変換結果[examples/test.ml](test.ml) を参考にしてください。

