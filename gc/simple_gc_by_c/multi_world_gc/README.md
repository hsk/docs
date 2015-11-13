# マルチワールドGC

書き掛け

## もくじ

- 1. <a name="C1"></a>[はじめに](#c1)
- 2. <a name="C2"></a>[アイディア](#c2)
- 3. <a name="C3"></a>[実装](#c3)
- 4. <a name="C9"></a>[参考文献](#c9)

## 1. <a name="c1"></a>[はじめに](#C1)

[new_world gc](../new_world) はレベルで世界を分けていました。
世界はネストしていて、階層構造にはなっていましたが、複数の世界が独立して存在する事はありませんでした。
ここでは、複数の世界を持つ事を考えます。

複数のメモリ領域があるということは、親の世界が子を複数持つ事が出来るという事です。
そして、子供同士がデータのやり取りが出来るかもしれません。
ヒープ空間を複数持てる事にしましょう。ゴミのでない計算機を作るのです。仮想的な計算機はバーチャルマシンですね。
バーチャルマシンを作ってそこにヒープ空間を作り、バーチャルマシンを切り替えて使う事にしましょう。

OlegのLightweight Monadic Regions<a name="r6"></a>[[6]](#6)に何が書いてあるかは把握してないのですが似ているかもしれません。と思ったけど、linier logicがどうとかあるので、違うか。

## 2. <a name="c2"></a>[アイディア](#C2)

新しい世界を作るときには、新しい世界を新たに作成しコンテキストを切り替えます。
新しい世界での計算が終わったらコンテキストを元に戻します。ただし、新しい世界はすぐに消さずに、元の世界に参照として残ります。
元の世界から参照されている間は、残り続けますが、参照がなくなるか、強制的に消されると新しい世界は無くなります。
複数の世界は互いに参照することはできますが、影響を与えられるのは自分の世界だけです。
計算が終わると、元の世界にバトンタッチして計算は終わるけれど、世界はすぐには終わりません。


そして、1の世界で新しいオブジェクトを作り、その後にもう一つ世界を作って、世界は終わらせずに、次のオブジェクトを作ってみましょう。

以下のようなイメージです。

	void test_multi_vm2() {
	  enum {frame_START, frame_SIZE, VM1,VM2,A, B, C, frame_END};
	  ENTER_FRAME_ENUM(frame);
	  frame[A] = gc_alloc_int(1);

	  assert(vm->heap_num==1);

世界を２つ作ります。

	  VM* tmp_vm = vm;
	  frame[VM1] = (Object*)vm_new();// 世界を作る
	  frame[VM2] = (Object*)vm_new();// 世界を作る
	  assert(vm->heap_num==3);

世界を移動します。

	  vm = (VM*)frame[VM1];// 世界を移動
	    assert(vm->heap_num==0);

計算して状態を保存します。

	    vm->record = test_int(frame[A]->intv);// 計算する
	    assert(vm->heap_num==1);

世界を移動します。

	  vm = (VM*)frame[VM2];// 世界を移動
	    assert(vm->heap_num==0);

計算して状態を保存します。

	    vm->record = test_int(frame[A]->intv);// 計算する
	    assert(vm->heap_num==1);

元に戻ります。

	  vm = tmp_vm;// 元に戻る
	  assert(vm->heap_num==3);

そして、元の世界にコピーを取ります。

	  frame[B] = vm_get_record((VM*)frame[VM1]);// コピーとる
	  frame[C] = vm_get_record((VM*)frame[VM2]);// コピーとる
	  assert(vm->heap_num==5);

２つの世界を消し、

	  frame[VM1] = NULL; // 世界を消す
	  frame[VM2] = NULL; // 世界を消す
	  printf("id change check.........\n");

コレクトするとヒープには3つのデータだけが残ります。

	  gc_collect();
	  assert(vm->heap_num==3);
	  LEAVE_FRAME(frame);
	}

世界がコレクトされて消えるときには、終了処理を走らせます。VMを解放するときにはそのVMを選択してgc_collectをすればよいでしょう。

## 3. <a name="c3"></a>[実装](#C3)

[gc.c](gc.c)に実装があります。

new_world gcのテストも含めるために、`PUSH_VM`と`POP_VM`というマクロを作りました。これを使えば、ネストした世界の構築をする事が出来ます。
また、`PUSH_VM`と`POP_VM`ではVMを何処にも保存しないので、GCが呼ばれると消えるため注意が必要です。

## 4. <a name="c4"></a>[考察](#C4)

ここで実装したものは、一度世界が参照に含まれると、所有権を持っている人が参照しなくなると世界は消えます。
複数の世界から参照する場合は、弱参照をすることになります。
複数の世界から参照する場合に強参照を可能にするにはやはり参照カウンタが必要で、循環参照の検出には世界のガーベジコレクションが必要となるでしょう。
どうも、ここで考えていた世界というものは、グリーンプロセスのようです。プロセスは他のプロセスの中味を触る事は出来ません。しかし通信をする事は出来ます。
データを取り出すには通信をする事で取り出すわけで、ここで考えていた世界は軽量なグリーンプロセスであるようです。
単純なものなので自分の世界に属しているかの判定に時間がかかりますが、より高速化することもできるでしょう。

Erlangのプロセスが優れているのは、他の関数型言語の実装は単一プロセス上で行われるのに対して、複数プロセス立ち上げによって１つ１つのメモリ空間は非常に小さくすることでGC時間を小さくします。多くの場合はおそらくメモリは動的に確保されても、プロセスが終了するまでGCされることはないのでしょう。

## 5. <a name="c9"></a>[参考文献](#C9)

- <a name="1"></a><a href="#r1">[1]</a> SML#

	http://www.pllab.riec.tohoku.ac.jp/smlsharp/ja/

- <a name="2"></a><a href="#r2">[2]</a> SML# ランタイム

	https://github.com/smlsharp/smlsharp/tree/v1.1.0/src/runtime

- <a name="3"></a><a href="#r3">[3]</a> SML# ランタイム　ファイナライザ

	https://github.com/smlsharp/smlsharp/search?utf8=%E2%9C%93&q=sml_set_finalizer

- <a name="4"></a><a href="#r4">[4]</a> DOT計算 Dependent Object Types Towards a foundation for Scala’s type system - Nada Amin Adriaan Moors Martin Odersky fool2012

	http://www.cs.uwm.edu/~boyland/fool2012/papers/fool2012_submission_3.pdf

- <a name="5"></a><a href="#r5">[5]</a> stackalloc hogelog

	https://github.com/hogelog/stackalloc

- <a name="6"></a><a href="#r6">[6]</a> Lightweight Monadic Regions - Oleg Kiselyov, Chung-chieh Shan

	http://okmij.org/ftp/Computation/resource-aware-prog/region-io.pdf
