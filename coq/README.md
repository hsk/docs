# プログラミング言語としてのCoq


そびえ立つ壁！登ってみたはいいけれど、もう一回登るのは嫌だ。
Coqなんて嫌いだ！

俺はかつて難しいと思いながらCoqを勉強し、そして、すっかり忘れてしまったのでした。
Coqはコンピュータ上で証明がかけて素晴らしい！
しかし、一度に扱う言語と証明用の言語をまとめて覚えようとすると学習階段が急すぎる！！

そこで、ここではCoqをベターOCamlなプログラミング言語として使ってみることにします。
Coqで作ったプログラムをOCamlから実行しながら関数型言語であるCoqを勉強してみます。

対象読者は、OCamlを理解している人です。

証明なんか、やってられるかー！っとｗ
いや、あとでやります。すいませんすいませんすいませんｗ

## [plus](plus)

足し算する関数plusをcoqで作り、ocamlにコンパイルして、ocamlcでメイン関数から呼び出します。

## [fact](fact)

factをcoqで作り、ocamlにコンパイルして、ocamlcでメイン関数から呼び出します。

## [calc](calc)

単純に四則演算するだけです。

## [let](let)

letをcoqで作り、ocamlにコンパイルして、ocamlcでメイン関数から呼び出します。

## [hello](hello)

coqでhello worldの文字列を作成し、ocamlからcoqの文字列を取り出して表示します。

## [link](link)

a.vの関数をb.vで呼び出し、b.vで定義された関数をmain.mlから呼び出します。

## [link2](link2)

abc/a.vの関数をb.vで呼び出し、b.vで定義された関数をmain.mlから呼び出します。
osxのバージョンの問題なのか、インストールの仕方が悪いのかちょっとよくわかりません。

## [lambda](lambda)

lambda計算をしてみます。
