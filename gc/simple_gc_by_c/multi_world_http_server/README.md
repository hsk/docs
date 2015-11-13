# マルチワールドHTTPサーバ

ここでは、multi world gcを使って、軽量プロセスによるwebサーバを作成します。
目標は、プログラムはDLLとして読み込み、軽量メモリ空間で実行し、処理が終わると解放する事です。
プログラムが古ければコンパイルし直します。
非同期IOが理想ですが、そこまではしません。するかもしれませんが。

	make

とすると自動的に

	http://localhost:8088/index.html

にアクセスし、index.htmlを表示します。

	http://localhost:8088/string

にアクセスすると、string.cがコンパイルして実行します。

## 1. とりあえず普通にDLLを作る

最初は普通にDLLを作ってみました。
参考URLはこの辺です。<a name="r1"></a>[[1]](#1)
Linuxとの互換性がっと思って、<a name="r2"></a>[[2]](#2) Linuxと同じように書いたら動いたのでこっちのほうがよいかなと。

	dylib: dylib.c dylib_main.c
		gcc -shared -fPIC -o dylib.so dylib.c
		gcc -o dylib dylib_main.c dylib.so
		./dylib

## 2. ダイナミックローディング

次は、ダイナミックにロードしてみましょう。

	dyload:
		gcc -shared -fPIC -o dylib.so dylib.c
		gcc -rdynamic -o dyload dyload.c -ldl

## 3. ダイナミックロードするDLLでGCを使用する

gcをするDLLを作って、test.soというDLLを作って読み込みます。

	dygc: gc.c test.c dygc.c test1.c
		gcc -shared -fPIC -o gc.so gc.c
		gcc -shared -fPIC -o test.so test.c gc.so
		gcc -rdynamic -shared -fPIC -o test1.so test1.c
		gcc -rdynamic -o dygc dygc.c -ldl gc.so
		./dygc

## 4. HTTPサーバを作る

socketを使ってHTTPサーバを作ってみます。
HTTPサーバプログラムの作成<a name="r3"></a>[[3]](#3)を参考に改造してみました。
DLLを読み込む機能を付け、標準出力をソケットに繋ぎ変えることで、CGIのような動作をDLLで行います。
動くと楽しいものです。

## 5. 使ってみて

GC付きのコードをちゃんと手で書くのはやっぱり面倒くさいですね。
最初は面白いんです。でも段々めんどくさくなってきます。
コンパイラからの出力なら良いのだろうけど、手動で使うのはちょっと、、、。
関数の開始と終了だけにマクロを書くのは仕方ないかもしれないですが、変数をイチイチ管理された領域に書き込む必要があるのが面倒くさいです。
管理された領域に書かないと、GCが起こった場合にだけ、突然謎のメモリエラーになってしまいます。

## 6. 参考文献

- <a name="1"></a>[[1]](#r1) Mac OSXでダイナミックライブラリdylibを作ったり使ったりする方法。

	https://blog.katty.in/4347

- <a name="2"></a>[[2]](#r2) Linuxで共有ライブラリsoを作ったり使ったりする方法。

	https://blog.katty.in/4346

- <a name="3"></a>[[3]](#r3) HTTPサーバプログラムの作成

	http://research.nii.ac.jp/~ichiro/syspro98/wwwserver.html

