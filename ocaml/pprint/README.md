# MLのプリティプリント

- [ ] 文章をまとめる。
- [ ] サンプルコードのファイル名を統一する

## 1. はじめに

  どうも、OCamlにはコメントを考慮した奇麗なプリティプリンタがCamlp4以外には存在していないようです。
  そして、CamlP4もイマイチのようです。

  時代はPPXとcompilerlibsの時代なのでうまくやりたい物です。

  ここでは、プリティプリントについて考えます。
  まず現状あるOCamlのプリティプリンタ周りの現状をまとめます。次にコメントの問題を解決する手法を提案し、実装してみます。
  最後にMLの固有の問題を洗い出し、解決方法を提案します。

## 2. 現状ある技術

### 2.1. Formatモジュール

  OCamlのFormatモジュールを使ってみます。

  分かり辛い改行とネストは以下のように書くとうまく行きます。

    let () =
      Format.printf "tes(@[<2>";
        Format.printf "@\ntes(@[<2>";
          Format.printf "@\ntes(@[<2>";
            Format.printf "@\naa";
            Format.printf "@\naa";
            Format.printf "@\naa";
          Format.printf "@]@\n)";
        Format.printf "@]@\n)";
      Format.printf "@]@\n)";
      Format.printf "@."

  出力結果

    tes(
      tes(
        tes(
          aa
          aa
          aa
        )
      )
    )

  ポイントは、改行は`@\n`で改行する事と、`@[<2>`と `@]` で括る事です。<2>はいくつネストするかを表します。

  ※ `\n` `@.` `@?`を使うとうまく動作しなくなるので注意が必要です。

  formatを使えば奇麗に書く事が出来ます。

### 2.2. 演算子の優先順位をつける

  優先順位を考慮したプリティプリントの方法は[OCamlチュートリアルのChapter 1 OCamlの基本、18. 1.8 Pretty-printing and parsing](http://ocaml.jp/Chapter%201%20OCaml%E3%81%AE%E5%9F%BA%E6%9C%AC#content_1_7)が参考になります。

### 2.3. 位置情報を元にコメントを挿入する

  色々弄っていたので、うごかないのですけど、以下のような感じで追加出来ます。

  - [ocaml/ocaml_src_reading/parsing](ocaml/ocaml_src_reading/parsing) OCamlのパーサにコメントを追加して、プリティプリントにコメントを追加する例

### 2.4. ocaml-jslib

  https://github.com/m2ym/ocaml-jslib

  JavaScriptのプリティプリントの例で、JavaScriptを構文解析しFormatを使って出力します。
  位置情報もついているので、あとは、コメントを付けるだけの状態です。
  コメントが消えるのと、演算子の優先順位の判定がないので実験にはちょうど良さそうです。

### 2.5. その他

  論文のリストを以下に示します。

  - http://belle.sourceforge.net/doc/hughes95design.pdf
  - http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
  - http://t0yv0.blogspot.com/2012/04/prettier-printer-in-ml.html
  - https://github.com/t0yv0/ocaml-pretty/blob/master/pretty_test.ml
  - http://okmij.org/ftp/continuations/PPYield/yield-pp.pdf
  - http://rgrig.blogspot.jp/2010/12/more-ocaml-pretty-printing.html

## 3. 基本的な技術

### 3.1. 改行情報を持ち回る

  簡単なプリティプリントを実装してみましょう。

  まず、以下のようにデータ定義をしましょう。

    type t =
      | Var of string
      | Tag of string * t list

  2つの構文があります。
  `a{1 b{ c d } 2}`は例えば以下のように表す事が出来ます:

    let e = Tag("a",[
      Var "1";
      Tag("b",[
        Var "c";
        Var "d"
      ]);
      Var "2"
    ])

  以下にプリティプリントを実装した関数ppを示します:

    let rec pp sp = function
     | Var(s) -> Printf.sprintf "%s%s\n" sp s
     | Tag(s,ts) ->
       let rec pps sp = function
         | [] -> ""
         | x::xs -> (pp sp x) ^ (pps sp xs)
       in
       Printf.sprintf "%s%s{\n%s%s}\n" sp s (pps ("  "^sp) ts) sp

  ppの第一引数spはネストの情報で文字列としてもちます。第二引数は構文木データです。
  この関数を使うには以下のようにして使います。

    let _ =
      let s = pp "" e in
      Printf.printf "%s\n" s

  実行すると、以下のように出力されます。

    a{
      1
      b{
        c
        d
      }
      2
    }

### 3.2. Formatモジュール

  改行情報を持ち回るのは大変なので、Formatのネストをうまくやってくれる仕組みを使うと便利です。しかし、挙動を変えたい場合は自作する必要があるでしょうから、作成してみます。
  Formatのprintf等では、型チェックもしますが、ここでは、プリティプリント用の機能のみを再現してみます。
  
#### 3.2.1. Formatを使った簡単な例

  ex3_2_1.ml

    open Format

    type t =
      | Var of string
      | Tag of string * t list

    let rec pp ppf = function
     | Var(s) -> fprintf ppf "%s" s
     | Tag(s,ts) ->
       let rec pps ppf = function
         | [] -> ()
         | x::xs -> fprintf ppf "@\n%a%a" pp x pps xs
       in
       fprintf ppf "%s{@[<2>%a@]@\n}" s pps ts

    let _ =
      let a = Tag("test",[Var "a";Var "b"]) in
      let a = Tag("test",[a;a]) in
      let a = Tag("test",[a;a]) in
      printf "%a\n" pp a

#### 3.2.2. Formatライブラリ自体の実装の仕組み

  ex3_2_2.jsで実装してみました。

    function print_int(n) {
      console._stdout.write(""+n);
    }
    var print_string = print_int;

    var Format = {
      level:0,
      levels:[],
      fprintf: function(fp,s) {
        var n = 2;
        var arg = arguments;
        return fp(s.replace(/(%.|@\[<([^>])>|@\n|@.)/g, function(a, b ,c) {
          switch (b.substr(0,2)) {
            case "%a":
              var f = arg[n++];
              return f(function(e){return e;},arg[n++]);
            case "%f":
              return arg[n++].toFixed(6);
            case "@.":
              return "\n";
            case "@?":
              return "";
            case "@[":
              var level = c|0;
              Format.level+=level;
              Format.levels.push(level);
              return "";
            case "@]":
              Format.level -= Format.levels.pop();
              return "";
            case "@\n":
              var str = "\n";
              for(var i = 0; i < Format.level;i++) str += " ";
              return str;

            default: return arg[n++];
          }

        }));
      },
      sprintf: function(s) {
        var args = Array.prototype.slice.call(arguments);
          args.unshift(function(i){return i;});
        return Format.fprintf.apply(this,args);
      },
      printf: function() {
        var args = Array.prototype.slice.call(arguments);
          args.unshift(print_string);
        Format.fprintf.apply(this,args);
      },
    };

      Format.printf("@[<2>tes(");
        Format.printf("@\n// test@\n@[<2>tes(");
          Format.printf("@\n@[<2>tes(");
            Format.printf("@\naa");
            Format.printf("@\naa");
            Format.printf("@\naa");
          Format.printf("@]@\n)");
        Format.printf("@]@\n)");
      Format.printf("@]@\n)");
      Format.printf("@.");

  ex3_2_2.ml

    module PP = struct
      let rec to_s a sp = function
        | "@["::os -> to_s a (sp ^ "  ") os
        | "@\n"::os -> to_s (a ^ "\n" ^ sp) sp os
        | "@]"::os ->
          to_s a
            (String.sub sp 0 ((String.length sp) - 2))
            os
        | o::os -> to_s (a^o) sp os
        | [] -> a

      let buf = ref []

      let put (s:string) =
        buf := s::!buf

      let puts ss =
        List.iter put ss

      let get () =
        let ls = List.rev !buf in
        buf := [];
        to_s "" "" ls
    end

    let _ =
      PP.puts["@[";"tes";"("];
        PP.puts["@\n";"@[";"tes";"(";];
          PP.puts["@\n"; "1"];
        PP.puts["@]";"@\n";")"];
      PP.puts["@]";"@\n";")"];
      let str = PP.get() in
      Printf.printf "%s\n" str

### 3.3. 演算子の優先順位

  ここでは具体的に、優先順位をつけたプログラムを作成してみましょう。

  優先順位を考慮したプリティプリント[OCamlチュートリアルのChapter 1 OCamlの基本、18. 1.8 Pretty-printing and parsing](http://ocaml.jp/Chapter%201%20OCaml%E3%81%AE%E5%9F%BA%E6%9C%AC#content_1_7)を参考に作成してみます。

  上のURLのアルゴリズムをそのまま使うと以下のようなケースで困ります。

    (a +  b ) * (c +  d) => (a +  b) *  (c  + d) // OK
    (a *  b ) + (c *  d) =>  a *  b  +   c  * d  // OK
    (a + "b") + (c +  d) =>  a + "b" +   c  + d  // NG JavaScriptの文字列結合結果が変わってしまう!
    (a :: b ) ::(c :: d) =>  a :: b  ::  c :: d  // NG リストの型が変わってしまう!

  左結合の場合は、右側は同じ演算子なら括弧をつけないといけないことがあり、
  右結合の場合は、右側が同じ演算子なら括弧をつけないことがあります。
  演算の順番を変更出来ない場合があるので、外さないのがよい訳です。

### 3.3.1 結合性を考慮した括弧付けアルゴリズム

  それでは結合性を考慮したアルゴリズムを考えてみましょう。

  基本的な考え方としては、見に来た人が自分より偉ければカッコをつけます。これを仮に勲章のアルゴリズム(order of algorithm)と呼ぶことにしましょう。

  普段はダラダラやっていても偉い将軍さまが来たら、カッコつけてみせるとうまくいくわけです。
  本当は同じ階級でも括弧が必要なら身分が上だぜと偉そうにする事で括弧を付けます。
  勲章の多さが身分を決めるように、優先順位を見る事でどちらが偉いか判定する事とします。

  勲章を１個多く付けて右側を見に行くと、右側に括弧がつきます。
  
    (a +  b ) * (c +  d) => (a +  b) *  (c  + d) // OK 既に強いので更にパワーアップしても変わらない
    (a *  b ) + (c *  d) =>  a *  b  +   c  * d  // OK どうあがいても勝てない
    (a + 'b') + (c +  d) =>  a + 'b' +  (c  + d) // OK
    (a :: b ) ::(c :: d) =>  a :: b  :: (c :: d) // NG aは値で、bとcはリストでdはリストのリストなのだ。

  勲章を１個多く付けて左側を見に行くと、左側に括弧がつきます。

    (a +  b ) * (c +  d) => (a +  b ) *  (c  + d) // OK
    (a *  b ) + (c *  d) =>  a *  b   +   c  * d  // OK
    (a + 'b') + (c +  d) => (a + 'b') +   c  + d  // NG 文字列にc足して、d足すのと、c+dして文字列化するのは違うんだ
    (a :: b ) ::(c :: d) => (a :: b ) ::  c :: d  // OK

  右結合のときは左側に行くときに勲章を１個多く付け、左結合のときは右側に行くときに勲章１個多く付けて見に行けば、

    (a +  b ) * (c +  d) => (a +  b) *  (c  + d) // OK
    (a *  b ) + (c *  d) =>  a *  b  +   c  * d  // OK
    (a + 'b') + (c +  d) =>  a + 'b' +  (c  + d) // OK
    (a :: b ) ::(c :: d) => (a :: b) ::  c :: d  // OK

  うまく行きます。知らない人が来たら偉い事にしましょう。お客様は神様です。

  以下の関数は二項演算子の括弧を付ける判定を行います:

    let order_of_bin op p =
      let (opp, l) = match op with
        | "::" -> (5, false)
        | "+"  -> (6,  true)
        | "*"  -> (7,  true)
        | _    -> (10, true)
      in
      let (p1, p2) = if l then (opp, opp + 1) else (opp + 1, opp) in
      let (lparen, rparen) = if p > opp then ("(",")") else ("","") in
      (lparen, rparen, p1, p2)

  演算子の種類から優先順位 opp と結合性 l を求めます。
  結合性 l から左と右に渡す優先順位 p1, p2 を決めます。
  pが opp より大きければ括弧を付けます。

  プリティプリントには、order_of_bin関数は以下のように組み込みます:

    let rec pp ppf t = 
      match t with
      | Var i -> fprintf ppf "%s" i
      | Bin(e1, op, e2) ->
        fprintf ppf "(%a %s %a)" pp e1 op pp e2

  ⇩⇩⇩⇩⇩

    let rec pp p ppf t = 
      match t with
      | Var i -> fprintf ppf "%s" i
      | Bin(e1, op, e2) ->
        let (lparen, rparen, p1, p2) = order_of_bin op p in
        fprintf ppf "%s%a %s %a%s" lparen (pp p1) e1 op (pp p2) e2 rparen



  pp関数に優先順位pを渡し、order_of_bin関数を加え、括弧の位置にlparen rparenを表示し、pp関数には優先順位p1, p2を渡します。

  関数ppは以下のようにして使います:

    let _ =
      let prog = [
        Bin(Bin(Var "a", "+" , Var  "b" ), "*" , Bin(Var "c", "+",  Var "d"));
        Bin(Bin(Var "a", "*" , Var  "b" ), "+" , Bin(Var "c", "*",  Var "d"));
        Bin(Bin(Var "a", "+" , Var "'b'"), "+" , Bin(Var "c", "+",  Var "d"));
        Bin(Bin(Var "a", "::", Var  "b" ), "::", Bin(Var "c", "::", Var "d"));
      ] in
      List.iter begin fun e ->
        printf "%a\n" (pp 0) e
      end prog

  実行してみましょう:

    (a + b) * (c + d)
    a * b + c * d
    a + 'b' + (c + d)
    (a :: b) :: c :: d

  思った通りに動いています。

  このプログラムは ex3_3.mlで全体を見る事が出来ます。

## 3.3.2 前置演算子と後置演算子

  前置演算子は、連続して書いても括弧はいらないので、


    - - 1
    string list list

  前置演算子、後置演算子は括弧を付ける必要はなさそうです。

    --1

  しかし、スペースを入れないとうまく行かなくなります。 ex3_3_b.mlで実装しています。TODOちゃんと実装する。


## 4. コメントの問題

  プリティプリントをする際のコメントには以下の問題点があります。

  1. コメント情報を何処に残すのか問題
      1. コメント情報を構文木に含める
      1. コメント情報は別に残して、位置情報から引き出す。
          1. 消えたトークン前後問題
  1. コメント何処にかかる問題

  コメント情報を何処に残してどう出力するのかの問題は構文木に含める手法と、構文木とは別に分けて保存し、ロケーション情報から復元する方法があります。
  また、構文木に全てのトークンが含まれない場合は、消えているトークンの前後どちらにコメントを追加するかが問題になります。

### 4.1. コメント情報を何処に残す

  コメント情報を何処に残してどう出力するのかの問題を詳しく見て行きましょう。

#### 4.1.1. 構文木に残す

  構文木にコメントを演算子の一種であるように、残しておけば、うまく行くかもしれません。
  構文木にコメントを残す場合の問題は、構文木の構造をそのまま活かせない点にあります。
  let in let inの連続をパターンマッチで判定する場合に、このコメント情報が残ると困ります。

#### 4.1.2. ロケーション情報に残す

  字句解析の段階でコメント情報の履歴を取っておきます。そして、印字の際にトークン情報を元に復元します。

#### 4.1.2.1. 消えたトークンの前後の問題

  ロケーションから復元する場合、消えたトークンの前後の問題があります。

    let a(*1*) = (*2*)1 in a

  `(*1*)`のコメントと`(*2*)`のコメントがありますが、構文木の情報は、

    Let("a", Int 1, Var "a")

  のようになりますが、ここで、仮にコメント情報を構文木に残した場合、

    Let("a", FCom("1", Int 1), Var "a")
    Let("a", FCom("2", Int 1), Var "a")

  のようにどちらも同じコメントになるので、分かりません。

  この問題は、トークン情報も残すことで解決出来るかもしれません。
  消えるトークンもまた、コメントの一部であると考えればよいのです。
  問題はどのトークンがコメントであるのかの判断でしょう。たとえば、括弧はそれに該当します。

### 4.2. コメントが何処にかかるかの問題

  たとえば、以下のようにコメントは下を修飾したり、横を修飾したりします。

    (* 関数のコメントみたいな物 *)
    let a = 1
    in 
    a (* 式に対するコメント *)

### 4.3. 堕胎な構文解析

  真面目に構文解析するから大変なので、コードハイライトをするには、字句解析のみで良いように、
  インデントを表すだけ言語としてパーサを作成して解決出来るならそのほうが楽でしょう。
  例えば、if() {}とfor(a;b;c) {}は同じ構文要素とみなすのです。
  function a () {} function() {}はどれも同じ構文要素と扱えばずっと楽に作成出来るでしょう。

## 5. コメントの問題の解決

  この章では、４章で見て来たコメントの問題点を解決する事を考えます。
  よりエレガントにコメントをプリントする事を考えれば、ロケーション情報からコメントを復元するのがよいでしょう。

### 5.1. 構文木にコメント情報を残す

  構文要素を全体を含めて、何番めにコメントがあるかを保存することで再現します。
  この方法は文法サイズに比例して、パーサの修正作業が必要になります。

#### 5.1.1. 構文要素の前後にコメント情報を残す

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

#### 5.1.2. 構文要素を子に持つ要素を作り、抽象構文木に残らないトークンの情報を保持する


  消えたトークンの前後問題は、例えば、以下の式で、

    let a(*1*) = (*2*)1 in a

  `(*1*)`のコメントと`(*2*)`のコメントがありますが、構文木の情報は、

    Let("a", Int 1, Var "a")

  のようになります。ここで、仮にコメント情報を構文木に残した場合、

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
      (* x = in に追加情報が必要 *)
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


  ここまで考えましたが、やはり、パーサを修正するのも大変そうです。
  パーサの修正を最小限にするには、トークン情報の前後から見るのがよい気がします。
  コメント情報以外のトークンも残すのがポイントな気がしてきます。

### 5.2. コメントをトークン列から生成する

  消えたトークンの問題を解決するには、コメントも同様に扱うと良さそうです。

  1 + 2 の2項演算子があった場合に、((1) + (2))のように括弧も残るかもしれません。
  主に、キーワードはコメント扱いすれば良さそうです。

    function a (hoge,huga) {
      return (hoge + huga);
    }

  これをトークンのリストにすると以下のようになります:

  1.  function
  2.  a
  3.  `(`
  4.  hoge
  5.  `,`
  6.  huga
  7.  `)`
  8.  `{`
  9.  return
  10. `(`
  11. hoge
  12. `+`
  13. huga
  14. `)`
  15. `;`
  16. `}`

----

  構文木の情報に位置情報を加えると以下のようになるでしょう:

    Fun(1,16,
      Var(2,"a"),
      [Var(4,"hoge");Var(6,"huga")],
      [
        Ret(9,15,Bin(11,13,Var(11,"hoge"),"+",Var(13,"huga")))
      ]
    )

  関数だけを取り上げましょう。

    /* a */
    function a (?) {
      ?
    }
    /* c */
    /* d */
    b

  このトークンリストは

  1.  function
  2.  a
  3.  `(`
  4.  `)`
  5.  `{`
  6.  `}`
  7.  b

----

  6つです。
  構文木は、

    Fun(1,6,
      Var(2,"a"),
      [],
      []
    )

  です。

  印字プログラムは以下のようになるでしょう:

    | Fun(sp,ep,Var(p1,x1),ss,es) ->
      pp_comment fp sp
      fprintf fp "function"
      fprintf fp "%s" xs
      fprintf fp "("
      pp_ss fp ss
      fprintf fp ")"
      fprintf fp "{"
      pp_es fp es
      fprintf fp "}"
      pp_comment fp sp


  ここに位置情報は含まれていませんが、前後関係をうまく印字したいなら、

  1. コメントはすぐ印字する。
  2. 印字されたものに、含まれていない保存してあるトークンがあれば、その手前に出力する。

----
  
  1.  /\*a\*/
  2.  /\*b\*/
  3.  function
  4.  /\*c\*/
  5.  a
  6.  /\*d\*/
  7.  `(`
  8.  /\*e\*/
  9.  `)`
  10.  /\*f\*/
  11.  `{`
  12.  `}`
  13.  /\*g\*/
  14.  b
  15.  /\*h\*/

----

  計算してみましょう。

    ""
    1,["/*a*/";"/*b*/";"function";"/*c*/";"a";"/*d*/";"(";"/*e*/";")";"/*f*/";"{";"}";"/*g*/";"b";"/*h*/"]
    1,3,"function",5,"a","(",")","{","}",14,"b"
    --
    "/*a*/";
    2,["/*b*/","function";"/*c*/";"a";"/*d*/";"(";"/*e*/";")";"/*f*/";"{";"}";"/*g*/";"b";"/*h*/"]
    2,3,"function",5,"a","(",")","{","}",14,"b"
    --
    "/*a*//*b*/";
    3,["function";"/*c*/";"a";"/*d*/";"(";"/*e*/";")";"/*f*/";"{";"}";"/*g*/";"b";"/*h*/"]
    3,3,"function",5,"a","(",")","{","}",14,"b"
    --
    "/*a*//*b*/";
    4,["/*c*/";"a";"/*d*/";"(";"/*e*/";")";"/*f*/";"{";"}";"/*g*/";"b";"/*h*/"]
    4,5,"a","(",")","{","}",14,"b"
    --
    "/*a*//*b*/function/*c*/";
    5,["a";"/*d*/";"(";"/*e*/";")";"/*f*/";"{";"}";"/*g*/";"b";"/*h*/"]
    5,5,"a","(",")","{","}",14,"b"
    --
    "/*a*//*b*/function/*c*/a";
    6,["/*d*/";"(";"/*e*/";")";"/*f*/";"{";"}";"/*g*/";"b";"/*h*/"]
    6,"(",")","{","}",14,"b"
    --
    "/*a*//*b*/function/*c*/a/*d*/";
    7,["(";"/*e*/";")";"/*f*/";"{";"}";"/*g*/";"b";"/*h*/"]
    7,"(",")","{","}",14,"b"
    --
    "/*a*//*b*/function/*c*/a/*d*/(";
    8,["/*e*/";")";"/*f*/";"{";"}";"/*g*/";"b";"/*h*/"]
    8,")","{","}",14,"b"
    --
    "/*a*//*b*/function/*c*/a/*d*/(/*e*/";
    9,[")";"/*f*/";"{";"}";"/*g*/";"b";"/*h*/"]
    9,")","{","}",14,"b"
    --
    "/*a*//*b*/function/*c*/a/*d*/(/*e*/)";
    10,["/*f*/";"{";"}";"/*g*/";"b";"/*h*/"]
    10,"{","}",14,"b"
    --
    "/*a*//*b*/function/*c*/a/*d*/(/*e*/)/*f*/";
    11,["{";"}";"/*g*/";"b";"/*h*/"]
    11,"{","}",14,"b"
    --
    "/*a*//*b*/function/*c*/a/*d*/(/*e*/)/*f*/{";
    12,["}";"/*g*/";"b";"/*h*/"]
    12,"}",14,"b"
    --
    "/*a*//*b*/function/*c*/a/*d*/(/*e*/)/*f*/{}";
    13,["/*g*/";"b";"/*h*/"]
    13,14,"b"
    --
    "/*a*//*b*/function/*c*/a/*d*/(/*e*/)/*f*/{}/*g*/";
    14,["b";"/*h*/"]
    14,14,"b"
    --
    "/*a*//*b*/function/*c*/a/*d*/(/*e*/)/*f*/{}/*g*/b";
    15,["/*h*/"]
    15
    --
    "/*a*//*b*/function/*c*/a/*d*/(/*e*/)/*f*/{}/*g*/b/*h*/";
    16,[]
    16
    --

  こんなルールで出力すればうまく行きそうな気もします。問題があるとすれば、括弧の対応とかかな。
  たぶん、全てのトークンリストと位置情報を取っておいて、出力するときにその情報と照合しながら出力すれば良さそうで、トークン番号を位置情報として持てばよさそうで、もしかすると、構文木には、位置情報すらいらないかもしれない。

  位置情報がどうしても必要なケースを考えてみると良さそうです。ネストした括弧が関係あるきがします。


  a(/*a*/((1))) というような式は、

    "a","(","/*a*/","(","(","1",")",")",")"
    "a","@[","(","1",")","@]"

    "a","(","/*a*/","(","(","1",")",")",")"
    "a","@[","(","1",")","@]"

  というリストだけで扱えそうな気もします。

  実装してみて、実験してみましょう。

  ex5_2_a.mlで書いてみましょう。
  段落処理等も、結構うまく行きました。結構というのは、段落とコメントの問題がまだうまく解決出来ていないからです。

  段落とコメントかぁ。

    @[a(@\n
      1@]@\n
    )@\n

    /*b*/a/**/(
      /*a*/1
    /**/)/*end*/

    @[/*b*/a/**/(@\n
      /*a*/1@]\n
    /**/)/*end*/

  ex5_2_b.mlを作ってみました。ex5_2_b.mlでかなりいいかんじ。
  でも問題は、

    // aa
    a(
      // aa
      a(
        2// a
        // a
        2// a
      )
      // aa
      a(
        2// a
        (2+1
      )// a
    ))

  ex_5_2_c.mlは対応する括弧のネストレベルを管理してこの問題を解決しています。
  予期していない括弧があれば、その括弧でネストレベルを１つ上げ、対応する括弧があったら１つ下げます。ネストレベルがあるうちは、括弧はうーん。

  より具体的な実装は ex5_2_d にあります。

  - a() と a{} が同じ意味である場合に、予期する出力は複数あり得てしまう。

  そこで、@( { ( @) というような指定でどちらも許すというような指定が出来るようにしてみた。
  このようにしても色々問題が出て来るので個別に色々と対応が必要である。

### 5.3. コメントの前後を判定する手法

  コメントが何処にかかるかの問題は、たとえば、

    (* 関数のコメントみたいな物 *)
    let a = 1
    in 
    a (* 式に対するコメント *)

  このようなコメントがあると、手前にかかるのか、後ろにかかるのか分からない事があるわけで、この問題を解決する必要があります。この解決は、コメントの手前にトークンがあるかどうかで判定出来ます。
  実は5.2の所で考え済みです。字句解析で、 文末にあるコメントは "@\n"を追加し、左側にスペース以外のトークンがあれば@を付けます。

      "// test";"@\n";
      "a";"(";
        "/* aa */";"@\n";
        "(";"2";"+";"1";")";"@";"// a";"@\n";
      ")";

## 6. ML固有の問題

  MLの場合は括弧でネストを作らずに、letでネストを作ったりするのでその辺がややこしいのでここではletを使った場合のプリティプリントについて考えます。

    type e =
      | Int of int
      | Let of string * e * e
      | Var of string

  のプリティプリントについて考えます。
  出来たらここに、コメントを追加して奇麗にプリティプリントする事を考えてみます。

### 6.1. 改行情報を持ち回る

  ex6_1.ml

    type e =
      | Int of int
      | Let of string * e * e
      | Var of string
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

    let _ =
      let e = Let("a",Let("b", Int 1, Var "b"),Let("c", Int 1, Var "b")) in
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

### 6.2. Formatを使って書く

  Formatを使ってみましょう:

  ex6_2.ml

    open Format

    type t =
      | Var of string
      | Let of string * t * t

    let rec pp ppf = function
     | Var(s) -> fprintf ppf "%s" s
     | Let(s,(Let _ as ts),s2) ->
       fprintf ppf "@[<2>let %s = @\n%a@]@\nin@\n%a" s pp ts pp s2
     | Let(s,ts,s2) ->
       fprintf ppf "let %s = %a in@\n%a" s pp ts pp s2

    let _ =
      let a = Let("test",Var "a",Var "b")in
      let a = Let("test",a,a) in
      printf "%a\n" pp a

### 6.3. 演算子の優先順位

  FormatとLetと演算子の優先順位を組み合わせた例です:

  ex6_3.ml

    open Format

    type t =
      | Var of string
      | Let of string * t * t
      | Bin of t * string * t
      | Pre of string * t
      | Post of t * string

    let infixs =
      [
        "=",  (1, false);
        "+",  (6, true);
        "-",  (6, true);

        "/",  (7, true);
        "*",  (7, true);
      ]

    let prefixs =
      [
        "new", (8, true);
        "!",   (8, false);
        "-",   (8, false);
      ]

    let postfixs =
      [
        "++", 9;
        "--", 9;
      ]

    let rec pp paren p ppf t = 
      match t with
      | Var i -> fprintf ppf "%s" i
      | Let(s,(Let _ as ts),s2) ->
        fprintf ppf "@[<2>let %s = @\n%a@]@\nin@\n%a"
          s (pp true 0) ts (pp true 0) s2
      | Let(s,ts,s2) ->
        fprintf ppf "let %s = %a in@\n%a" s (pp true 0) ts (pp true 0) s2

      | Pre(op, e1) ->

        let (p1,ident) = (List.assoc op prefixs) in
        let paren = paren && p1 < p in

        if paren then fprintf ppf "(";
        fprintf ppf " %s" op;
        if ident then fprintf ppf " ";
        pp true p1 ppf e1;
        if paren then fprintf ppf ")"

      | Post(e1, op) ->

        let p1 = (List.assoc op postfixs) in
        let paren = paren && p1 <= p in

        if paren then fprintf ppf "(";
        fprintf ppf " %s%a" op (pp true (p1 - 1)) e1;
        if paren then fprintf ppf ")"

      | Bin(e1, op, e2) ->
        let (p1, l) = (List.assoc op infixs) in
        let paren = paren && (if l then p1 <= p else p1 < p) in
        if paren then fprintf ppf "(";
        pp paren (if l then p1 - 1 else p1 + 1) ppf e1;
        fprintf ppf " %s " op;
        pp true p1 ppf e2;
        if paren then fprintf ppf ")"

    let _ =
      let a = Let("test",Var "a",Var "b")in
      let b = Bin(Var "a","*",Var "b") in
      let c = Bin(b,"*",b) in
      let c = Bin(c,"*",Var "a") in
      let a = Let("test",a,c) in
      printf "%a\n" (pp true 0 ) a

  これも、書きました。どこかで。

### 6.4. コメントを含んだLetとの組み合わせ

### 6.5. 構文解析と組み合わせる
