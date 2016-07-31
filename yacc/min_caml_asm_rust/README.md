# min\_caml\_asm\_rust

min\_caml\_asm\_rust は、rustで実装した、min-camlのx86バックエンド部分のみを取り出したプログラムです。

lalrpopを用いて、パースし、mlsファイルを読み込んでx86アセンブラに変換しgccでコンパイルして実行します。

## install & make & test

lalrpopをcargoでインストール

    multirust run stable cargo lalrpop

パスを通します。

	make

で、ビルド＆実行します。

