# plus

とりあえず、足し算関数を作ってみます。

plus.vというファイルに以下のように書きます。

```
Require Import ExtrOcamlNatInt.

Definition plus (n : nat)(m : nat) : nat :=
  n + m.


Extraction "plus.ml" plus.
```

Requireうんたらは、OCamlに出力するときに、nat(自然数)をintにしてエクスポートしますよという意味です。
Extraction "plus.ml" plus.でplus関数を、plus.mlに出力してよという命令です。

Makefileに以下のように書きます:


```
$ coqc plus.v
```

とすると、

plus.mli plus.mlの２つのファイルが出力されます。

plus.mlの中身は以下のとおりで:

```
(** val add : int -> int -> int **)

let rec add = (+)

(** val plus : int -> int -> int **)

let plus n m =
  add n m
```

plus関数が出来上がっていることがわかります。

main.mlを書き:

```
let () =
  Printf.printf "%d\n" (Plus.plus 10 20)
```

コンパイル&実行で、

```
$ ocamlc plus.ml main.ml -o plus
$ ./plus
30
```

となれば成功です。

わかったら、Makefile書いて自動化しましょう:


```
all:
	coqc plus.v
	rm -rf plus.mli
	ocamlc -o plus plus.ml main.ml
	./plus

clean:
	rm -rf *.glob *.vo .*.aux plus.mli plus.ml *.cm* plus
```

makeでcoqcでコンパイルして、plus.mliとplus.mlが出来上がります。plus.mliはインターフェイスファイルなので、消してしまってmain.mlから呼び出して実行します。

make cleanで余計なファイルは削除です。


## 覚えるべきこと

DefinitionがOCamlのletで、:=で定義し、.が最後にくると覚えましょう。
なぜ、Coqのキーワードがこんなに長いのかというと、Coqはプログラムを扱う言語であるからです。
証明するためのキーワードと扱う言語のキーワードは分けて考えているからのようなのですが、とにかく長いので覚えづらいかもしれないのです。ということで、サングラスをかけた男がデファインしてハクションしていると覚えましょう。なんで、サングラスをかけた男かというとまぁ、覚えやすいからです。

```
Definition plus (n : nat)(m : nat) : nat :=
  n + m.
```

エクスポートの仕方はこれ見て思い出せばOKです。おまじないは最初は覚えんでいいっす。
