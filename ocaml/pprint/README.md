# MLのプリティプリント

## 概要

    type e =
      | Int of int
      | Let of string * e * e
      | Var of string

  のプリティプリントについて考えます。
  出来たらここに、コメントを追加して奇麗にプリティプリントする事を考えてみます。

## 1. はじめに


  どうも、OCamlには奇麗なプリティプリンタが存在していないようです。
  おそらく、普通じゃない言語であるので、いるのかどうか分かりませんが、プリティープリント職人も手を出し辛い言語なのではないかと思います。逆に学術的な研究に使われているので、研究対象として面白い素材なのかもしれません。
  そこで、ここでは、プリティプリントについて考えます。

  まず、抽象構文木にしてから印字する場合、演算子の優先順があるので括弧を生成する必要が出てきます。
  解決するには、括弧を全部に付ける方法と、優先順位を考慮して括弧を付けて出力する方法があります。

  優先順位を考慮したプリティプリントについては[OCamlチュートリアルのChapter 1 OCamlの基本、18. 1.8 Pretty-printing and parsing](http://ocaml.jp/Chapter%201%20OCaml%E3%81%AE%E5%9F%BA%E6%9C%AC#content_1_7)に詳しくあります。

  また、表を使って演算子の括弧を付けるプリティプリントは[GomaJの出力](https://github.com/hsk/gomaj/blob/master/src/gen_java.ml)が参考になります。

  論文 http://belle.sourceforge.net/doc/hughes95design.pdf
  http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf

  http://t0yv0.blogspot.com/2012/04/prettier-printer-in-ml.html
  https://github.com/t0yv0/ocaml-pretty/blob/master/pretty_test.ml

  http://okmij.org/ftp/continuations/PPYield/yield-pp.pdf

  http://rgrig.blogspot.jp/2010/12/more-ocaml-pretty-printing.html

  1. 括弧の生成
      1. 全部に括弧を付ける
      2. 優先順位を考慮して括弧を付ける
  2. コメントの問題
      1. コメント情報を何処に残すのか問題
          1. 構文木に含める
          2. コメント情報は別に残して、ロケーション情報から引き出す。
      2. 消えたトークン前後問題
      3. コメント何処にかかる問題


  もう一つはコメントの問題で、コメント情報を何処に残すのかという問題があり、これは構文木に含める手法と、
  構文木とは別に分けて保存し、ロケーション情報から復元する方法があります。
  消えたトークンの前後問題は、例えば、以下の式で、

    let a(*1*) = (*2*)1 in a

  `(*1*)`のコメントと`(*2*)`のコメントがありますが、構文木の情報は、

    Let("a", Int 1, Var "a")

  のようになりますが、ここで、仮にコメント情報を構文木に残した場合、

    Let("a", FCom("1", Int 1), Var "a")
    Let("a", FCom("2", Int 1), Var "a")

  のようにどちらも同じコメントになるので、分かりませんという問題です。
  コメントが何処にかかるかの問題は、たとえば、

    (* 関数のコメントみたいな物 *)
    let a = 1
    in 
    a (* 式に対するコメント *)

  このようなコメントがあると、手前にかかるのか、後ろにかかるのか分からない事があるわけで、この問題を解決する必要があります。

  この文書では、コメントを構文木に情報を残す方法を考え、解決を試みます。
  消えたトークン前後問題と、コメント何処にかかる問題の２つがあります。

  2章では、簡単なプリティプリントをする実装を行います。

  3章では、構文木にコメントを残す手法を考え、4章では、消えたトークンを残す手法を5章で コメントの前後を判定する手法について考えます。6章では問題ですのでやめます。

## 2. 簡単なプリティプリント


  簡単なプリティプリントを実装してみましょう。

  まず、以下のようにデータ定義をしましょう。

    type e =
      | Int of int
      | Let of string * e * e
      | Var of string


  3つの構文があります。
  `let a = let b = 1 in b in let c = a in c`は例えば以下のように表す事が出来ます:

    let e = Let("a",Let("b", Int 1, Var "b"),Let("c", Var "a", Var "c"))

  Letの最初の式は1つネストをさげ、次の式はネストを下げないようにして、再帰的に処理する事で実装出来ます。
  以下にプリティプリントを実装した関数ppを示します:

    let rec pp sp = function
      | Int(i) ->
        Printf.sprintf "%s%d\n" sp i
      | Var(x) ->
        Printf.sprintf "%s%s\n" sp x
      | Let(x,e1,e2) ->
        Printf.sprintf "%slet %s =\n%s%sin\n%s"
          sp
          x
          (pp (sp ^ "  ") e1)
          sp
          (pp sp e2)

  ppの第一引数spはネストの情報で文字列としてもちます。第二引数は構文木データです。
  この関数を使うには以下のようにして使います。

    let _ =
      let s = pp "" e in
      Printf.printf "%s\n" s

  実行すると、以下のように出力されます。

    let a =
      let b =
        1
      in
      b
    in
    let c =
      1
    in
    b

  簡単ですね。

## 3. 構文木にコメントを残す

  コメント情報は、手前と、後ろからの関連付けでうまく行きそうな気がします。

    type e =
      | Int of int
      | Let of string * e * e
      | Var of string
      | FCom of string * e
      | RCom of e * string 

  FComで手前にコメントを残す事を表し、RComで後ろにコメントを残す事を表します。
  しかし、残念ながらこの方法では、以下のコメントAとBの箇所にコメントを埋め込む事が出来ません。

    let(*1*)a(*2*)= 1 in a

## 4. 消えたトークンを残す


  消えたトークンの前後問題は、例えば、以下の式で、

    let a(*1*) = (*2*)1 in a

  `(*1*)`のコメントと`(*2*)`のコメントがありますが、構文木の情報は、

    Let("a", Int 1, Var "a")

  のようになりますが、ここで、仮にコメント情報を構文木に残した場合、

    Let("a", FCom("1", Int 1), Var "a")
    Let("a", FCom("2", Int 1), Var "a")

  のようにどちらも同じコメントになるので、分からなくなり問題です。

  これをOCamlのppx、Javaのアノテーションのような感じで、式の上から中味にかぶせる方式を取る事で、対応出来ます。

    type e =
      | Int of int
      | Let of string * e * e
      | Var of string
      | UCom of string list * e

  UComのデータがあった場合、以下のように処理をする事でコメントをいかなる場所にも埋め込む事が可能です。

      | UCom([fi;bi],Int(i)) ->
        Printf.sprintf "%s%s%d%s\n" sp fi i bi
      | UCom([fx;bx],Var(x)) ->
        Printf.sprintf "%s%s%s%s\n" sp fx x bx
      (* let x = b in c *)
      (* x = in が追加情報が必要　*)
      (* Let of string * e * e *)
      | UCom([flet;fx;feq;beq;fin;bin;blet],Let(x,e1,e2)) ->
        Printf.sprintf "%s%slet%s%s%s=%s\n%s%sin%s\n%s%s%s"
          sp flet
          fx x
          feq beq
          (pp (sp ^ "  ") e1)
          fin bin
          sp (pp sp e2)
          blet
      | UCom(ls,e) -> 
        Printf.sprintf "%s%s" (String.concat "" ls) (pp sp e)


  具体的な使用例を以下に示します:

    let _ =
      let e =
        Let("a",
          Let("b",
            UCom(["(*fint*)";"(*bint*)"],Int 1),
          UCom(["(*fvar*)";"(*bvar*)"],Var "b")),
          UCom(["(*Flet*)\n";"(*Fx*)";"(*Feq*)";"(*Beq*)";"(*Fin*)";"(*Bin*)";"(*Blet*)"],
          Let("c",
            Int 1,
            Var "b")))
      in
      let s = pp "" e in
      Printf.printf "%s\n" s

  この手法の問題は、全てのデータについて書かなくては行けない点にあります。

  次に、前後に残すパターンと上から指定するパターンを混在させるパターンを書いてみましょう。

    type e =
      | Int of int
      | Let of string * e * e
      | Var of string
      | FCom of string * e
      | RCom of e * string
      | UCom of (string list)* e

    let chop str = String.sub str 0 (String.length str - 1)
    let rec pp sp = function
      | Int(i) ->
        Printf.sprintf "%s%d\n" sp i
      | Var(x) ->
        Printf.sprintf "%s%s\n" sp x
      | Let(x,e1,e2) ->
        Printf.sprintf "%slet %s =\n%s%sin\n%s"
          sp
          x
          (pp (sp ^ "  ") e1)
          sp
          (pp sp e2)
      | FCom(fe,e) ->
        Printf.sprintf "%s%s\n%s" sp fe (pp sp e)
      | RCom(e,fe) ->
        Printf.sprintf "%s%s\n" (chop (pp sp e)) fe
      | UCom([fx;bx;],Let(x,e1,e2)) ->
        Printf.sprintf "%slet%s%s%s=\n%s%sin\n%s"
          sp
          fx x bx
          (pp (sp ^ "  ") e1)
          sp
          (pp sp e2)
      | UCom(ls,e) -> 
        Printf.sprintf "%s%s" (String.concat "" ls) (pp sp e)

    let _ =
      let e =
        Let("a",
          Let("b",
            FCom("(*fint*)",Int 1),
          RCom(Var "b","(*bvar*)")),

          RCom(
          FCom("(*Flet*)",
          UCom(["(*Fc*)";"(*Bc*)"],
          Let("c",
            RCom(FCom("(*f1*)",Int 1),"(*r1*)"),
            RCom(FCom("(*fb*)",Var "b"),"(*rb*)")))),"(*Rlet*)"))
      in
      let s = pp "" e in
      Printf.printf "%s\n" s

  この例では、追加情報が必要な部分にのみ追加出来ています。


  パーサを修正するのも大変そうです。パーサの修正を最小限にするには、やはり、トークン情報の前後から見るのがよい気がします。コメント情報以外のトークンも残すのがポイントな気がしてきます。


## 位置情報を元にしたコメントとトークン情報の保存と印字

  パースする場合に、lexerが補助となるコメント、およびトークンを覚えてくれればパーサの変更無しにコメントを残す事が出来ます。

  

## 5. コメントの前後を判定する手法

  コメントが何処にかかるかの問題は、たとえば、

    (* 関数のコメントみたいな物 *)
    let a = 1
    in 
    a (* 式に対するコメント *)

  このようなコメントがあると、手前にかかるのか、後ろにかかるのか分からない事があるわけで、この問題を解決する必要があります。この解決にはおそらく、コメントの手前にトークンがあるかどうかで判定出来ます。また、構文木によっては、後ろにかけるしかない場合も有り得るでしょう。その時は、後ろにかけましょう。

## 6. 実装する

  それでは、方針は決まりましたので、実装してみましょう。

