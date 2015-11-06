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
- object.c object.h
- objspace.c objspace.h
- obstack.c
- splay.c splay.h
- main.c
- smlmain.c

##	gcc main1.c error.c

エラーは単独で使えます。様々なレベルのエラー出力やフックなどがあります。

## 	gcc main1.c splay.c

splay.cも単独で使えます。ツリー情報を扱います。

## gcc main1.c obstack.c error.c

スタック情報を表します。errorにのみ依存しています。


## control.c objspace.c object.c

		control.c \ コントローラ プログラムのスレッドやスタックの状態等を管理します。
		objspace.c \ オブジェクトの空間 GCの共通ライブラリな感じ
		object.c \ オブジェクトそのもの オブジェクトのダンプや、作成、レコードの作成、比較、コピーなど

この3つは今まで出て来たソースが全て必要です。からんでいます。




## main.cとSMLmain

main.cはSMLmainとリンクしてメイン関数として動きます。


	メインのプログラムは初期化を呼び出して初期化し、
	アプリケーションコードのメイン関数を呼び出し、
	プログラムが終わったら終了処理を呼び出します。