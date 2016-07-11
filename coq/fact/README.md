# fact

今回は、let recに対応する、Fixpointを覚えるある。
Fixpointとはすなわち、不動点、動かない点であり、Y combinatorのポールグレハムであるｗ
山の不動が蟻地獄でうぎゃー。吸い込まれたら上から落っこちてきたイメージを持てば多分忘れない。


fact.v:
```
Require Import ExtrOcamlNatInt.

Fixpoint fact (n : nat) :=
  match n with
  | O => 1
  | S m => n * fact m
  end.

Extraction "fact.ml" fact.
```

とにかく再帰してる。
Coqのnatは実はOとSの集合でペアノ数であるので、上のように書けるわけです。
ペアノ数はOが0でS(O)が1、S(S(O))が2と再帰的に自然数を定義したもので、数のない所に数を作ったものです。
こいつらの足し算や掛け算を再帰的に定義していけば様々な計算ができて素晴らしいということらしいのですけど、使う人的にはとにかく、OCamlにコンパイルして計算できれば良いので、

Makefileを書き換えて、

```
all:
	coqc fact.v
	rm -rf fact.mli
	ocamlc -o fact fact.ml main.ml
	./fact

clean:
	rm -rf *.glob *.vo fact.mli fact.ml *.cm* .*.aux fact
```

main.mlを書いて

```
let () =
  Printf.printf "%d\n" (Fact.fact 10)
```

```
$ make
coqc fact.v
rm -rf fact.mli
ocamlc -o fact fact.ml main.ml
./fact
3628800$ 
```

動いた。

## まとめ

let rec はFixpoint不動点コンビネータのポールグレハム。山の不動が蟻地獄に吸い込まれて空から落っこちてくる。
なんじゃそれ。
