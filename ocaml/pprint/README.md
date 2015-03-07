# OCamlにおけるプリティプリントの基本的な技術

## 1. はじめに

  どうも、OCamlにはコメントを考慮した奇麗なプリティプリンタがCamlp4以外には存在していないようです。
  そして、CamlP4もイマイチのようです。

  時代はPPXとcompilerlibsの時代なのでうまくやりたい物です。

  ここでは、プリティプリントについて考えます。
  まず現状あるOCamlのプリティプリンタ周りの現状をまとめます。次にコメントの問題を解決する手法を提案し、実装してみます。
  最後にMLの固有の問題を洗い出し、解決方法を提案します。


  一体何をしようとしているかというと、プログラムにコメント情報を残しつつ抽象構文木を操作して元に戻すというような事をすることで自由自在にプログラムでプログラムを操作したいのです。現状はトランスレータを書くとコメントは消えてしまいます。TypeScriptはある程度残りますが、行末のコメントは残りますが、式中のコメントは残りません。
  何よりソースコードが10万行とかあってううです。PureScriptは消えています。

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
  <center>ex2_1.ml</center>

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

  以下に簡単な例を示します。

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

  <center>ex3_2_1.ml</center>

#### 3.2.2. Formatライブラリ自体の実装の仕組み

  以下のように実装してみました:


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

  <center>ex3_2_2.ml</center>

  実行すると以下の結果が得られます:

    tes(
      tes(
        1
      )
    )


  また、ex3_2_2.jsにはJavaScriptでも実装してみた例があります。

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

  仮に勲章を１個多く付けて右側を見に行くと、右側に括弧がつきます。
  
    (a +  b ) * (c +  d) => (a +  b) *  (c  + d) // OK 既に強いので更にパワーアップしても変わらない
    (a *  b ) + (c *  d) =>  a *  b  +   c  * d  // OK どうあがいても勝てない
    (a + 'b') + (c +  d) =>  a + 'b' +  (c  + d) // OK
    (a :: b ) ::(c :: d) =>  a :: b  :: (c :: d) // NG aは値で、bとcはリストでdはリストのリストなのだ。

  仮に勲章を１個多く付けて左側を見に行くと、左側に括弧がつきます。

    (a +  b ) * (c +  d) => (a +  b ) *  (c  + d) // OK
    (a *  b ) + (c *  d) =>  a *  b   +   c  * d  // OK
    (a + 'b') + (c +  d) => (a + 'b') +   c  + d  // NG 文字列にc足して、d足すのと、c+dして文字列化するのは違うんだ
    (a :: b ) ::(c :: d) => (a :: b ) ::  c :: d  // OK

  右結合のときは左側に行くときに勲章を１個多く付け、左結合のときは右側に行くときに勲章１個多く付けて見に行けば、

    (a +  b ) * (c +  d) => (a +  b) *  (c  + d) // OK
    (a *  b ) + (c *  d) =>  a *  b  +   c  * d  // OK
    (a + 'b') + (c +  d) =>  a + 'b' +  (c  + d) // OK
    (a :: b ) ::(c :: d) => (a :: b) ::  c :: d  // OK

  うまく行きます。勲章のアルゴリズムではこの方法を使います。また、知らない人が来たら偉い事にしましょう。お客様は神様です。

  以下のorder_of_bin関数は二項演算子の括弧を付ける判定を行います:

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

  以下のようにすることでプリティプリント関数ppにorder_of_bin関数を組み込む事が出来ます:

  before:

    let rec pp ppf t = 
      match t with
      | Var i -> fprintf ppf "%s" i
      | Bin(e1, op, e2) ->
        fprintf ppf "(%a %s %a)" pp e1 op pp e2

  after:

    let rec pp p ppf t = 
      match t with
      | Var i -> fprintf ppf "%s" i
      | Bin(e1, op, e2) ->
        let (lparen, rparen, p1, p2) = order_of_bin op p in
        fprintf ppf "%s%a %s %a%s" lparen (pp p1) e1 op (pp p2) e2 rparen



  具体的にはpp関数に優先順位pを渡し、order_of_bin関数を加え、括弧の位置にlparen rparenを表示し、pp関数には優先順位p1, p2を渡します。

  関数ppは以下のように使います:

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

  思った通りに動きました。テストも書いておきましょう:

    let _ =
      let test t s =
        fprintf str_formatter "%a" (pp 0) t;
        assert (s = (flush_str_formatter()))
      in
      test (Bin(Bin(Var "a", "+" , Var  "b" ), "*",  Bin(Var "c", "+",  Var "d"))) "(a + b) * (c + d)";
      test (Bin(Bin(Var "a", "*" , Var  "b" ), "+",  Bin(Var "c", "*",  Var "d"))) "a * b + c * d";
      test (Bin(Bin(Var "a", "+" , Var "'b'"), "+",  Bin(Var "c", "+",  Var "d"))) "a + 'b' + (c + d)";
      test (Bin(Bin(Var "a", "::", Var  "b" ), "::", Bin(Var "c", "::", Var "d"))) "(a :: b) :: c :: d";

  このプログラムは ex3_3.ml にあります。

### 3.3.2 前置演算子と後置演算子の括弧とスペース

  前置演算子や後置演算子は、連続して書いても括弧は必要ありません。
  
    string list list
    - -1

  しかし連続して書くと意味が異なってしまいます。

    --1
    stringlistlist

  スペースを含める場合と含めない場合が存在します。

  スペースを省略してよいケースは、記号の演算子が連続して現れない場合だけです。
  演算子が記号でありかつ内包する演算子が記号でないときはスペースを付けなくてよいのです。

  また、括弧を付けないと行けない事もあります:

    -(1 + 1)

  単純な項なら省略してよいのですが、組み合わされた式なら括弧が必要です。この括弧を付けは、3.3のいわゆる勲章のアルゴリズムを使えばうまく行くでしょう。
  スペースは、再帰的に検索する必要があります。

  優先順位を表に持っておき:

    let prefixs =
      [
        "not",   (9,  true);
        "-",     (9, false);
      ]

    let postfixs =
      [
        "--",   (10, false);
        "list", (10,  true);
      ]

  以下のようなプログラムで括弧とスペースを無駄なく印字出来ます:

    let rec order_of_pre op p e =
      let (opp,ident) = List.assoc op prefixs in
      let (lparen, rparen) = if p > opp then ("(",")") else ("","") in
      let space = ident || start_op opp e in
      (lparen, rparen, opp, if space then " " else "")
    and start_op p = function
      | Var _ -> false
      | Bin _ -> false
      | Pre(op, _) -> not (snd (List.assoc op prefixs))
      | Post(t,op) -> let (lparen,_,_,_) = order_of_post op p t in lparen = "("
    and order_of_post op p e =
      let (opp, ident) = (List.assoc op postfixs) in
      let (lparen, rparen) = if p > opp then ("(",")") else ("","") in
      let space = ident || end_op opp e in
      (lparen, rparen, opp, if space then " " else "")
    and end_op p = function
      | Var _ -> false
      | Bin _ -> false
      | Pre(op, t) -> let (lparen,_,_,_) = order_of_pre op p t in lparen <> "("
      | Post(t,op) -> not (snd (List.assoc op postfixs))

  実行すると例えば、以下のような結果が得られます:

    not 0
    not not a
    not (0 + 2)
    -0
    - -a
    -(0 + 2)
    -(-0 + 2)
    a list
    a list list
    (0 + 2) list
    a--
    a-- --
    (0 + 2)--
    -a--
    (-a)--
    -(-a)--
    (-a--)--
    (- -a)--
    -a-- --

  後置演算子で記号を使う例が思いつかなかったのですが、前か後ろかの違いなので必要かどうかは置いておいて必要になってから悩まなくて済むように考えました。
  そう！数学は必要かどうかは関係ないのです。
  
  この実装はex3_3_2.mlで全体を見る事が出来ます。
