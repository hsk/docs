# ペアノ数とOCaml

論文って難しいですよね。簡単だって人は読まなくてよいと思います。多分必要ありません。この文章は論文わかんねーって人の為の物です。きっと偉い先生方が書いて喜んでるわけですから、嬉しいはずなんですよね。一般庶民にはその嬉しさが分かってないだけで。

```
	n ::= Z | S(n)
```

というBNFで自然数を表す事が出来ますと言われると。これどういう事だ？ってなるのが一般庶民の感覚ではないでしょうか？

でもまぁ、落ち着いてこのペアノ数のプログラムをOCamlを使って書いてみましょう。

まず、構文木を定義してみます。

```
type n =
  | Z
  | S of n
```

こんなんですかね？Zか、Sにnが入るらしいので、更に入っているように書けば良いっぽい。
使い方としては以下のような感じで書けるはず:

```
let zero = Z
let one = S(Z)
let two = S(S(Z))
```

プリントする関数も書いてみましょう。

```
open Format

let rec print fp = function
  | Z -> fprintf fp "Z@?"
  | S(n) -> fprintf fp "S(%a)@?" print n
```



さて、BNFらしいので、パーサが書けますよね。

書いてみようじゃないの、、、。

parser.mly

```
%token S Z
%token LPAREN RPAREN
%token EOL

%start main
%type <Ast.n> main

%%

main:
  | n EOL { $1 }

n:
  | Z                   { Ast.Z }
  | S LPAREN n RPAREN   { Ast.S( $3 ) }
```

こんなんでしょうか？EOLは改行で、まぁ試すのに必要です。tokenやstart, typeも必要なので書き足して、Astを返すアクションを加えたと。

字句解析も必要ですよね。定義しましょう。

lexer.mly

```
{
open Parser
}

rule token = parse
  | [' ' '\t']     { token lexbuf }
  | 'S'            { S }
  | 'Z'            { Z }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '\n'           { EOL }
```

出来たぞー。スペースは読み飛ばしが必要で、改行はEOLを返すと。

でもって、メインの処理があれば動かせるのかなぁ:

main.ml

```
open Format

let rec eval = function
  | Ast.Z -> 0
  | Ast.S(n) -> (eval n) + 1

let main =
  let lexbuf = Lexing.from_channel stdin in
  while true do
    let ast = Parser.main Lexer.token lexbuf in
    fprintf std_formatter "%a@." Ast.print ast;
    fprintf std_formatter "%d@." (eval ast)
  done
```

標準入力を受け取って、Ast.printで出力してます。
それだけだと、良くわからないので、評価関数evalを作って評価してみました。

この評価規則は以下のように書けるのかも:

```
Z
- (e-zero)
0

S(n)
----- (e-succ)
1 + n
```

よくわかってないので、間違ってたらすいません。


あとは、ビルドできればいいはず:

Makefile

```
all:
	ocamlyacc parser.mly
	ocamllex lexer.mll
	rm parser.mli
	ocamlc ast.ml parser.ml lexer.ml main.ml -o nat

clean:
	rm -f *.cm* parser.ml lexer.ml nat
```

`make all`でocamlyaccでparse.mlを作り、ocamllexでlexer.mlを作り、parser.mliは消しちゃって、ocamlcでコンパイルして、natを作ります。
`make clean`すれば、余計なファイルは消せます。なんか、バッチファイルじゃないかっていう気もするんだけど、OCaml速いし、まいいじゃん。

実行してみましょう。

```
./nat
Z
Z
0
S(Z)
S(Z)
1
S(S(Z))
S(S(Z))
2

Fatal error: exception Parsing.Parse_error
```

いい感じじゃないでしょうか。

## さらに

evaml1ディレクトリと、evalnatディレクトリには更に、足し算とかけ算を追加したものがあります:

```
n ::= Z | S(n)
e ::= n | e + e | e * e
```

evalnatディレクトリにはif文があるような以下のBNFに対するプログラムがあります。

```
i ::= int
b ::= bool
v ::= i | b
e ::= i | b | e op e | if e then e else e
op ::= + | - | * | <
```

こうやって、大きくして行く事に慣れると論文のBNFは楽しくなるんじゃないでしょうか？

## まとめ

```
n ::= Z | S(n)
```

というたったこの１つの式でこれだけのプログラムを表せました。素晴らしい事です。人類の英知じゃあ！！！

偉い先生方はこの辺がもう、体に染み付いてるんでしょうねぇ。これが、一般庶民の我々も分かってしまえば、もう、バッチリです。

## ふゅーちゃーわーく

今回は`S(Z)`と書いて、1と評価するようにしましたが、1と書くと、S(Z)になるようにパーサを拡張したり、足し算や引き算を定義してみると面白いかも、かけ算や割算を定義したりね。

テストも書いてないから、正しく動くか分かんないし、テストを闇雲に書くのもどうなのかと。
そう考えて行くと、ちゃんと証明してあればいい。証明は美しくまとまったテストといえるのかも。テストだけでは、完全に正しく動く事を保証できないけど、証明されていれば、正しく動く事が保証出来る。証明が間違えていれば、保証出来てない事になる。

つまり、正しい証明は１００万個のテストケースに勝る。それだけ証明は強力なテストなので作るのは難しいのである。だから、世の中には沢山の証明されていない問題が残っていて、証明されると莫大な賞金が与えられる。それだけの価値があるのである。たしかに証明は難しいが、大いなる価値のある最強のテストなのである。

## 参考文献


1. [プログラミング言語の基礎概念](http://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/)
2. [OCaml-manual-3.06-ja](http://ocaml.jp/archive/ocaml-manual-3.06-ja/manual026.html)
