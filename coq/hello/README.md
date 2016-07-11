# hello world

In Coq, Hello world even BlackBox.

現在のCoqではハローワールドすらブラックボックスです。
ということで、とにかく、将来的にはブラックボックスではなくなるであろうことは、Haskellのブラックボックスがブラックボックスでなくなったことから洞察されます。

しかしながら、とにかく、プログラミング言語としてCoqから文字列を出力するのは大変らしく、ググってみたところ、文字列を使えるようにして、Coqの文字列は文字のリストなので、変換関数作ればいいみたいです。

hello.v:

```
Require Import ExtrOcamlString.

Require Import String.
Open Scope string_scope.

Definition msg := "Hello world!".

Extraction "hello.ml" msg.
```

main.ml:

```
let rec string_of_coqstring cstr = 
  let buf = Buffer.create 256 in
  let rec cnv = function
    | [] -> Buffer.contents buf
    | c::cstr ->
      Buffer.add_char buf c;
      cnv cstr
  in
  cnv cstr

let () =
  Printf.printf "%s\n" (string_of_coqstring Hello.msg)
```

Makefileは省略でこれで、うまくいきます。Buffer使ったから早いよと。

## まとめ

CoqのHello Worldは今はブラックボックスです。そのうちきっと、ブラックボックスではなくなるでしょう。
