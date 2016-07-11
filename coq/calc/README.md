# calc

ここで覚えてもらうのは、OCamlのtypeであるInductionだ。
Inductionは長いので、in duct ハクション。ダクトとは、配管であり、暗渠であるらしいので、タモリさんコックの帽子をかぶってウロウロしてて、暗渠に入って行ってハクションです。それが、Inductionです。

四則演算してみます。

calc.v

```
Require Import ExtrOcamlNatInt.

Inductive e : Type :=
  | EInt : nat -> e
  | EAdd : e -> e -> e
  | ESub : e -> e -> e
  | EMul : e -> e -> e.

Fixpoint eval (e : e) : nat :=
  match e with
  | EInt n => n
  | EAdd a b => (eval a) + (eval b)
  | ESub a b => (eval a) - (eval b)
  | EMul a b => (eval a) * (eval b)
  end.

Extraction "calc.ml" eval.
```

OCamlと若干見た目が違うけど、綺麗に書けますですよ。ofとか書くより、関数として書いたほうが嬉しいでしょ。
ということで、


Coqのnatはペアノ数なので、割り算を使うのは初心者としては面倒くさいです。
何かインポートすれば使えるようになるのでしょうけどとりあえず何もせず使える演算子は、+,-,*です。
+,-,*の演算子を使ってevalしてみました。


main.mlは:

```
open Calc
let () =
  Printf.printf "%d\n" (eval (EAdd(EInt 10, EInt 20)))
```

Makefileでたくさんの変数を使うとわかりづらくなると思うのですが、
単純な書き換えが面倒になってきたので変数を使う事にしました。

Makefile:

```
TARGET=calc
all:
	coqc $(TARGET).v
	rm -rf $(TARGET).mli
	ocamlc -o $(TARGET) $(TARGET).ml main.ml
	./$(TARGET)

clean:
	rm -rf *.glob *.vo .*.aux $(TARGET).mli $(TARGET).ml *.cm* $(TARGET)
```

コマンドラインから、

```
$ make
coqc calc.v
rm -rf calc.mli
ocamlc -o calc calc.ml main.ml
./calc
30
```

終わったら

```
$ make clean
```

## まとめ

ここで覚えるのは、Induction が代数型定義です。タモリさんウロウロ、コックの帽子かぶって暗渠に入って、ハクションで、カタカタです。

