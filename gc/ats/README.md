# GCをできるだけ短く書こう計画

## イントロダクション

多くのプログラミング言語ではメモリ管理を参照カウンタ方式あるいはガーベジコレクションを用いてプログラマがメモリ管理を気にする必要がありません。

非常に便利なGCですが、高速に実装するのは難しい物があります。

多くのアルゴリズムは関数型言語を使って記述すると奇麗に書く事が出来ます。
同様に、GCの実装を関数型言語で書けばより抽象的に書く事が出来るかもしれません。

しかしながら、GCの実装を関数型言語で書くためには、１つ大きな問題があります。
その問題とは、与えられた配列の中のメモリを使ってデータを構築することです。

様々な言語がありますが、多くの関数型言語にはこの機能がありません。
幸いにして、ATS2はその機能を持っています。

我々はATS2をGCを奇麗に書く事を検討し、提案します。

1章では、最初にATSでのリストを使ったパターンマッチを使ってみます。
2章では、非常に簡単な、インタプリタやスタックマシンを実装してみます。
3章では、危険なキャストを使って割り当てられた配列の中からデータを作成する関数を作成し、使ってみます。
4章では、環境がすべて、リスト上にあるだけの簡単なGCを実装します。
5章では、関数とリストがある言語を実装します。
6章では、より複雑なデータ構造に対するGCを実装します。
7章では、リストとintのみが存在する言語のビットマップGCを実装します。


- [ats_basic](ats_basic) ではATSでGCを作るための基本的な技術をまとめます。
- [mark_and_sweep_in_c](mark_and_sweep_in_c) ではCを使って簡単なMark & Sweepのプログラムを作ってみます。
- [mark_and_sweep](mark_and_sweep) ではMLを使って簡単なMark & Sweepのプログラムを作ってみます。
- [setgc](setgc) ではMLを使って簡単なBitmapではなく外部にオブジェクト用のマーク情報の集合を持つ事でGCを行います。


## bitmap gc

## 参考文献

An Efficient Non-Moving Garbage Collector for Functional Languages

http://www.pllab.riec.tohoku.ac.jp/papers/icfp2011UenoOhoriOtomoAuthorVersion.pdf

SMLSharpのBitmapGC

https://github.com/smlsharp/smlsharp/blob/master/src/runtime/heap_bitmap.c

簡単な mark & sweepの例

https://github.com/munificent/mark-sweep/blob/master/main.c


https://github.com/authorNari/g1gc-impl-book/blob/master/precise.re

https://github.com/authorNari/minigc
