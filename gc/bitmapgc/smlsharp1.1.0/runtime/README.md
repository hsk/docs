# SML\# 1.0.3 ランタイムを読む


# ソース一覧

- README.md このファイル
- Makefile とりあえずコンパイル

- smlsharp.h SMLSharpの定義
- heap.h ヒープ定義

- メモリ管理プログラムどれか１つを使う必要があります。
	- heap_bitmap.c
	- heap_cheney.c
	- heap_malloc.c
	- heap\_otomo.c heap\_otomo.h

- error.c エラー 定義はsmlsharp.hにあります。
- control.c control.h frame.h
- exn.c
- init.c
- object.c object.h intinf.h
- objspace.c objspace.h
- obstack.c
- prim.c prim.h intinf.h gmp使ったりしている。
- splay.c splay.h
- main.c
- smlmain.c

##	gcc main1.c error.c

エラーは単独で使えます。様々なレベルのエラー出力やフックなどがあります。

## 	gcc main1.c splay.c

splay.cも単独で使えます。ツリー情報を扱います。

## gcc main1.c exn.c error.c

exnもerrorにのみ依存しています。外部の関数を登録したりします。

## gcc main1.c obstack.c error.c

スタック情報を表します。errorにのみ依存しています。


## -lgmp heap_malloc.c control.c objspace.c object.c

	-lgmp
		control.c \ コントローラ プログラムのスレッドやスタックの状態等を管理します。
		heap_malloc.c GC、ヒープメモリ管理
		objspace.c \ オブジェクトの空間 GCの共通ライブラリな感じ
		object.c \ オブジェクトそのもの オブジェクトのダンプや、作成、レコードの作成、比較、コピーなど

この４つは今まで出て来たソースが全て必要です。からんでいます。

gmpも必要になります。

## init.c

		init.c mainから引数を受け取って保存した後、初期化します。

		コントローラを作り
		ヒープを作り
		オブジェクトスペースを作って

		オブジェクトスペースを開放し
		ヒープを開放し
		コントローラを開放します。

## prim.cを使うにはコマンドラインの引数が必要なので、初期化が必要です。



		prim.c プリミティブの演算をするための関数です。


## main.cとSMLmain

main.cはSMLmainとリンクしてメイン関数として動きます。


	メインのプログラムは初期化を呼び出して初期化し、
	アプリケーションコードのメイン関数を呼び出し、
	プログラムが終わったら終了処理を呼び出します。