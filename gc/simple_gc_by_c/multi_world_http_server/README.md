# Multi World HTTP Server

## Table of Contents

- <a name="rintro"></a>[Introduction](#intro)
1. <a name="rc1"></a>[DLLを作る](#c1)
1. <a name="rc2"></a>[ダイナミックローディング](#c2)
1. <a name="rc3"></a>[ダイナミックロードするDLLでGCを使用する](#c3)
1. <a name="rc4"></a>[HTTPサーバを作る](#c4)
1. <a name="rc5"></a>[考察](#c5)
1. <a name="rc6"></a>[参考文献](#c6)

## <a name="intro"></a>[Introduction](#rintro)

どうも、世界を複数作って作るGCはErlangの軽量プロセスのようなものです。
ここでは、Multi World GCを使って、軽量プロセスによるWebサーバを作成します。
目標は、プログラムはDLLとして読み込み、軽量メモリ空間で実行し、処理が終わると解放する事です。
プログラムが古ければコンパイルし直します。
非同期IOが理想ですが、そこまではしません。

	make

とすると自動的に

<http://localhost:8088>

にアクセスし、[pandoc.c](pandoc.c)を実行して[README.md](README.md)をHTMLに変換し表示します。

<http://localhost:8088/string>

にアクセスすると、string.cがコンパイルして実行します。

## 1. <a name="c1"></a>[DLLを作る](#rc1)

最初は普通にDLLを作ってみました。
参考URLはこの辺です。<a name="r1"></a>[[1]](#1)
Linuxとの互換性がっと思って、<a name="r2"></a>[[2]](#2) Linuxと同じように書いたら動いたのでこっちのほうがよいかなと。

	dylib: dylib.c dylib_main.c
		gcc -shared -fPIC -o dylib.so dylib.c
		gcc -o dylib dylib_main.c dylib.so
		./dylib

## 2. <a name="c2"></a>[ダイナミックローディング](#rc2)

次は、ダイナミックにロードしてみましょう。

	dyload:
		gcc -shared -fPIC -o dylib.so dylib.c
		gcc -rdynamic -o dyload dyload.c -ldl

## 3. <a name="c3"></a>[ダイナミックロードするDLLでGCを使用する](#rc3)

gcをするDLLを作って、test.soというDLLを作って読み込みます。

	dygc: gc.c test.c dygc.c hello.c
		gcc -shared -fPIC -o gc.so gc.c
		gcc -shared -fPIC -o test.so test.c gc.so
		gcc -rdynamic -shared -fPIC -o hello.so hello.c
		gcc -rdynamic -o dygc dygc.c -ldl gc.so
		./dygc

## 4. <a name="c4"></a>[HTTPサーバを作る](#rc4)

socketを使ってHTTPサーバを作ってみます。
HTTPサーバプログラムの作成<a name="r3"></a>[[3]](#3)を参考に改造してみました。
DLLを読み込む機能をつくり、標準出力をソケットに繋ぎ変えることで、CGIのような動作をDLLで行います。
繋ぎ変えるにはfilenoを使って低水準のハンドルを取得し、dupを使ってハンドルをコピーして保存し、dup2で置き換えます。

	  int stdoutno = fileno(stdout);
	  int back_stdoutno = dup(stdoutno);
	  dup2(sockfd, stdoutno);

処理が終わったら、fflushで出力をフラッシュして、dup2でバックアップを取っていた物に標準出力を戻し、バックアップのハンドルは閉じます。

	  fflush(stdout);
	  dup2(back_stdoutno, stdoutno);
	  close(back_stdoutno);

動くと楽しいものです。

ソケットを使ったサーバは、直ぐに閉じなくて困るのでオプションを設定してみました。

	  int val = 1;
	  setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &val, sizeof(val));

	  struct linger ling;
	  ling.l_onoff = 1;
	  ling.l_linger = 0;
	  printf("set %d\n", setsockopt(sockfd, SOL_SOCKET, SO_LINGER, &ling, sizeof(ling)));

シグナルに対応するなどもしてますが、当たり前ですけど軽量プロセス故に1個死ぬと全部死にます。
HTTPモジュールなので仕方ないですね。みたいな雰囲気を醸し出しており面白いものがあります。
シグナルは<a name="r5"></a>[[5]](#5)を参考にしました。

## 5. <a name="c5"></a>[考察](#rc5)

さまざまな、プログラムを作ってみました。

1. [hello.c](hello.c)はなにもしていないのに、printfだけで動きます。[hello](hello)
1. [test.c](test.c)はなにもしてませんが、GCをごにょごにょしています。[test](test)
1. [string.c](string.c)は文字列生成関数がないと不便だと思って文字列用関数を作って使ってみてました。[string](string)
1. [pandoc.c](pandoc.c)はsystem関数を使ってpandoc<a name="r4"></a>[[4]](#4)を呼び出します。
	要するにこのファイル[(README.md)](README.md)を表示します。
	Haskellが動くWebサーバですよw <http://localhost:8088/pandoc> にアクセスすれば見れるのです。
	楽しい。このファイルを書き換えるだけで、内容が変わる。不思議です。
1. [calc.c](calc.c)は四則演算の構文木を作り、計算します。[calc](calc)

非常に面白いですね。SML#でもWebアプリをSML#で作っていたのもうなずけます。

しかし、GC付きのコードをちゃんと手で書くのはやはり手間がかかります。
コンパイラから出力したいところです。やはりコンパイラを作るしかない！
関数の開始と終了だけにマクロを書くのは仕方ないですが、変数を管理された領域に書き込む必要があります。
さもないと、GCが起こった場合にだけ、突然謎のメモリエラーになってしまいます。
動的言語なら、ポーティングを書くのがめんどくさいみたいになる所です。やっぱり、Rubyのような保守的なGCは楽で良いですねぇ。
とはいえ、自分が作ろうとしている言語はコンパイラ言語です。Cを書かずにCのソースをポート出来れば良いのです。
However、完全な仕組みを理解し、それを使う事が目的であったので結構満足出来ました。

## 6. <a name="c6"></a>[参考文献](#rc6)

- <a name="1"></a>[[1]](#r1) Mac OSXでダイナミックライブラリdylibを作ったり使ったりする方法。

	<https://blog.katty.in/4347>

- <a name="2"></a>[[2]](#r2) Linuxで共有ライブラリsoを作ったり使ったりする方法。

	<https://blog.katty.in/4346>

- <a name="3"></a>[[3]](#r3) HTTPサーバプログラムの作成

	<http://research.nii.ac.jp/~ichiro/syspro98/wwwserver.html>

- <a name="4"></a>[[4]](#r4) Pandocユーザーズガイド

	<http://sky-y.github.io/site-pandoc-jp/users-guide/>

- <a name="5"></a>[[5]](#r5)【Android NDK】sigsegvをフックする【cocos2d-x】

	<http://qiita.com/kuuki_yomenaio/items/08781e4778df3928bb9b>
