# コンパイラを作る

# 1. OCamlで簡単なトランスレータを書く

まずは、四則演算の計算をして表示するだけのCへのトランスレータを書いてみましょう。

## test1.ml

	type e =
	  | EInt of int
	  | EAdd of e * e
	  | EMul of e * e
	  | ESub of e * e
	  | EDiv of e * e
	  | ELet of string * e * e

	let rec p fp e =
	  match e with
	  | EInt(i) -> Format.fprintf fp "(%d)" i
	  | EAdd(e1,e2) -> Format.fprintf fp "(%a+%a)" p e1 p e2
	  | EMul(e1,e2) -> Format.fprintf fp "(%a*%a)" p e1 p e2
	  | ESub(e1,e2) -> Format.fprintf fp "(%a-%a)" p e1 p e2
	  | EDiv(e1,e2) -> Format.fprintf fp "(%a/%a)" p e1 p e2

	let comp fp e =
	  Format.fprintf fp "#include <stdio.h>\n";
	  Format.fprintf fp "#include \"gc.h\"\n";
	  Format.fprintf fp "int main() {\n";
	  Format.fprintf fp "  printf(\"%%d\\n\", %a);\n" p e;
	  Format.fprintf fp "  return 0;\n";
	  Format.fprintf fp "}\n"

	let _ =
	  comp Format.std_formatter (EAdd(EInt 1,EInt 2));

これは四則演算の構文木をCにトランスレートするプログラムで、以下のようなC言語のプログラムを文字列として標準出力に出力します。

	#include <stdio.h>
	#include "gc.h"
	int main() {
	  printf("%d\n", ((1)+(2)));
	  return 0;
	}

これをファイルに保存するには、

	ocaml test.ml > test.c

のようにすれば、保存出来ます。

## Makefile

以下のようにMakefileに書いて、

	gc1:
		ocaml test1.ml > test1.c
		gcc gc.c test1.c -o test1
		./test1

以下のように実行すると、

	make gc1

コンパイルと、実行がされて最終的に

	3

と1+2の結果が表示されます。

# 2. gc.cをgc.hとgc.cとtest2.cに分ける。

[mark\_and\_sweep](../mark_and_sweep)で書いたC言語のプログラムは、１ソースでメイン関数まで書いていましたが不便でした。
そこでここでは、gc.hとgc.cとtest1.cの３つに分けましょう。

## Makefile

メイクファイルには以下のように追加して

	gc2: gc.h gc.c test2.c
		gcc gc.c test2.c -o test2
		./test2

以下のように実行しましょう。

	make gc2

テストプログラムがダダっと動けばOKです。


# 3. gcをトランスレータに組み込む

OCamlのプログラムにGCを組み込んでみましょう。

## test3.ml

	type e =
	  | EInt of int
	  | EAdd of e * e
	  | EMul of e * e
	  | ESub of e * e
	  | EDiv of e * e

	let rec p fp e =
	  match e with
	  | EInt(i) -> Format.fprintf fp "(%d)" i
	  | EAdd(e1,e2) -> Format.fprintf fp "(%a+%a)" p e1 p e2
	  | EMul(e1,e2) -> Format.fprintf fp "(%a*%a)" p e1 p e2
	  | ESub(e1,e2) -> Format.fprintf fp "(%a-%a)" p e1 p e2
	  | EDiv(e1,e2) -> Format.fprintf fp "(%a/%a)" p e1 p e2

	let comp fp e =
	  Format.fprintf fp "#include \"gc.h\"\n";
	  Format.fprintf fp "#include <stdio.h>\n";
	  Format.fprintf fp "\n";
	  Format.fprintf fp "void _main(){\n";
	  Format.fprintf fp "  enum {FRAME_START, FRAME_SIZE, A, FRAME_END};\n";
	  Format.fprintf fp "  ENTER_FRAME_ENUM();\n";
	  Format.fprintf fp "  frame[A]= gc_alloc_int(%a);\n" p e;
	  Format.fprintf fp "  printf(\"%%d\\n\", frame[A]->ints[0]);\n";
	  Format.fprintf fp "  LEAVE_FRAME();\n";
	  Format.fprintf fp "}\n";
	  Format.fprintf fp "\n";
	  Format.fprintf fp "int main() {\n";
	  Format.fprintf fp "  gc_init();\n";
	  Format.fprintf fp "  _main();\n";
	  Format.fprintf fp "  gc_free();\n";
	  Format.fprintf fp "  return 0;\n";
	  Format.fprintf fp "}\n"

	let _ =
	  comp Format.std_formatter (EAdd(EInt 1,EInt 2));


わざわざ、GC領域に変数を取ってint値を保存してみました。

## Makefile

以下の記述を追加しましょう。

	gc3:
		ocaml test3.ml > test3.c
		gcc gc.c test3.c -o test3
		./test3

以下のように実行すると

	make gc3

test3.cが生成され、3が表示されます。

## test3.c

ちゃんと、gcを初期化し、完全なGCをつかって用意した変数を使ったCソースにトランスレート出来ています。
素晴しい！

	#include "gc.h"
	#include <stdio.h>

	void _main(){
	  enum {FRAME_START, FRAME_SIZE, A, FRAME_END};
	  ENTER_FRAME_ENUM();
	  frame[A]= gc_alloc_int(((1)+(2)));
	  printf("%d\n", frame[A]->ints[0]);
	  LEAVE_FRAME();
	}

	int main() {
	  gc_init();
	  _main();
	  gc_free();
	  return 0;
	}

# 4. k正規化

変数を付けえるようにしたいのですが、めんどくさくなるので先にk正規化してしまいましょう。
無駄に変数を使ってしまいますが、まずは動けば良いのです。


## コラム

ところで、高速なコンパイラを作るにはポインタを保存するアドレスの把握が必要です。
保存するアドレスは生きてたり死んでいたりするはずです。
変数はレジスタか、スタック上のアンマネージな領域か、マネージな領域に保存される必要がありレジスタが溢れた場合にポインタだった場合は
マネージされた位置へ書き込む必要があります。

GCをする時は、マネージ領域だけを見る訳ですが、既に死んでいるオブジェクトが含まれているかもしれません。
それは関数を抜ければ開放されるはずですが、生きている時もあるでしょう。でもそのポインタ値はチェックされます。

で、マネージされたポインタとマネージされないポインタをレジスタ割り付けするときまで保持する必要があります。

LLVMではどうかとかある訳です。




