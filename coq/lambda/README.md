# lambda

λ計算してみましょう。
作ってみて困ったところが幾つかありました。

整数の比較は、

```
Require Export Arith.EqNat.
```

をして、beq_natを使うこと。


boolを&&でつなぐ場合などは、


```
Require Export Bool.Bool.
```

としてBoolの関連するファイルを読み込む必要があります。

詳しく知らないので、構造的なデータの比較も、再帰的な比較関数を書きました。

なんだか、ずいぶんRequireが多いなと思うのですが、なぜ多いのかというと、Coqはもともと証明をするのが目的の言語なので、プログラム以外に証明コードが大量に含まれているので、モジュール1つ1つは小さくなりがちです。
よく使うファイルは共通ライブラリに入れるといいのでしょうけど、独自で頑張ろうと思うと細かい指定が必要になります。

Bool.Boolは https://coq.inria.fr/library/Coq.Bool.Bool.html
Arith.EqNatは https://coq.inria.fr/library/Coq.Arith.EqNat.html にドキュメントがあるので読みましょう。

型から関数を探すツールもあるかもということで、、、。
