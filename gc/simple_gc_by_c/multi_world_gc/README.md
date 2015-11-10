# マルチワールドGC

## もくじ

- 1. <a name="C1"></a>[はじめに](#c1)
- 2. <a name="C2"></a>[とりあえず実装](#c2)
- 3. <a name="C3"></a>[改良する](#c3)
- 4. <a name="C4"></a>[ファイナライザ](#c4)
- 5. <a name="C9"></a>[参考文献](#c9)

## 1. <a name="c1"></a>[はじめに](#C1)

[new_world gc](../new_world) はレベルで世界を分けていました。
世界はネストしていて、階層構造にはなっていましたが、複数の世界が独立して存在する事はありませんでした。
ここでは、複数の世界を持つ事を考えます。

複数のメモリ領域があるということは、親の世界が子を複数持つ事が出来るという事です。
そして、子供同士がデータのやり取りが出来るかもしれません。
とにかく考えるべきは、IDを持ち、レベルではなくIDで管理する事でしょう。

計算世界のIDが同じであれば、同じ世界であり、IDが違う時は違う世界です。

## 2. <a name="c2"></a>[とりあえず実装](#C2)

[gc.c](gc.c)に実装があります。もうとりあえず作りながら考えましょう。
たぶん、新しい世界を作るときには、新しい世界を新たにぶち立てて、古い世界は一度別の所に移動させます。
そして、終わったら元に戻します。ただし、新しい世界はすぐに消さずに、元の世界に参照として残ります。
元の世界から参照されている間は、残り続けますが、参照がなくなるか、強制的に消されるとようやく消えます。
複数の世界が参照することはできますが、影響を与えられるのは自分の世界だけです。
計算が終わると、元の世界にバトンタッチして計算は終わるけれど、世界はすぐには終わらない。

ということであるのです。

`world_count`をグローバルに持ち作られた世界をカウントアップしますこれが行き着いた場合はとりあえず考えません。
`gen_world_id`関数が`id`を生成して返します。`world_id`が現状世界のIDです。元の世界のIDはローカル変数に取ります。

ObjectHeaderの構造体に`level`の代わりに`id`を持ちます。

    <   int id;

グローバル変数に`world_count`と`world_id`と`frame_bottom`を追加します。

    < int world_id;
    < int world_count;
    < Frame* frame_bottom;

マーク時にワールドIDでなければマークしないようにします。

    <   if (head->marked || head->id != world_id) return;
    ---
    >   if (head->marked) return;

マークするルート集合は新しく作り出したフレームだけにします。

    <   while(frame != frame_bottom) {
    ---
    >   while(frame) {

スイープの関数には、世界が終わった時のためにidを渡すようにします。

    < void gc_sweep(int id) {
    ---
    > void gc_sweep() {

スイープのループ中にidがワールドIDでなくなれば終了します。

    <     if((*object)->id != world_id) break;

ID指定があればIDを設定します。

    <       if(id) {
    <         printf("id change\n");
    <         printf("id change %d -> %d\n", (*object)->id, id);
    <         (*object)->id = id;
    <       }

世界が終わった時の特別なGCを作ります。移り住む世界のIDを渡します。

    < void gc_collect_end_world(Object* data, int world_id) {
    <   int prev_num = heap_num;
    <   gc_mark_object(data);
    <   gc_sweep(world_id);
    <
    <   heap_max = prev_num * 2;
    <
    <   debug("Collected %d objects, %d remaining.\n", prev_num - heap_num,
    <          heap_num);
    < }

世界を開始するマクロを追加して使います。`tmp`にはバックアップを取るフレーム名を指定します。

    < #define NEW_WORLD(tmp) \
    <   int tmp##_world = world_id;
    <   world_id = gen_world_id(); \
    <   Frame* tmp = frame_bottom;


    < #define END_WORLD(tmp,root) \
    <   gc_collect_end_world(root, tmp##_world); \
    <   frame_bottom = tmp; \
    <   world_id = tmp##_world;

通常のスイープは引数を`0`で呼び出します。

    <   gc_sweep(0);
    ---
    >   gc_sweep();

オブジェクトを作成した場合はidを保存します。

    <   head->id = world_id;

	< int gen_world_id() {
	<   return world_count++;
	< }


初期化時は`world_count`を`0`に、フレームボトムは`NULL`にして、`world_id`には`gen_world_id`を呼び出して最初のidを降ります。

    <   world_count = 0;
    <   world_id = gen_world_id();
    <   frame_bottom = NULL;

テストコードを追加します。

	< void test_multi_world() {
	<   enum {FRAME_START, FRAME_SIZE, A, B, C, D, FRAME_END};
	<   ENTER_FRAME_ENUM();
	<
	<   frame[A] = gc_alloc_int(1);
	<
	<   NEW_WORLD(frame_tmp1);
	<     frame[B] = test_int(frame[A]->intv);
	<   END_WORLD(frame_tmp1,frame[B]);
	<
	<   NEW_WORLD(frame_tmp2);
	<     frame[C] = test_int(frame[B]->intv);
	<   END_WORLD(frame_tmp2,frame[C]);
	<
	<   NEW_WORLD(frame_tmp3);
	<     frame[D] = test_int(frame[C]->intv);
	<   END_WORLD(frame_tmp3,frame[D]);
	<
	<   printf("id change check.........\n");
	<   gc_collect();
	<   LEAVE_FRAME();
	< }

メインにテストを追加します。

    <   printf("------------- test multi world\n");
    <   gc_init();
    <   test_multi_world();
    <   gc_free();

これで作業は完了です。

お、すぐ出来たぞとおもったら、new_worldのほうを修正していた！！
持って来たらエラーだ。マクロの問題か。修正完了っと。すぐ動いてしまいました。

しかし、これだと、計算終わったら同じ世界に統合されてしまうのでマルチにした意味がないw
うーん。毎回IDは増えるだけだな。うーとどうしたら良いんだ、、、。
なんとなくわかったのは、世界が混在している場合、

	heap_list

	1
	3
	3
	3
	1
	1
	1
	1
	2
	2
	2
	2
	1
	1
	1
	1

のような状態になりかねないという事です。ここでヒープを見る場合、同じヒープを使っていると、全部チェックしなくてはなりません問題が現れます。

そこで、ヒープのバックアップが必要でしょう。テストもそれなりのものを考えないといけません。

## 3. <a name="c3"></a>[改良する](#C3)

2章でとりあえず作った仕組みでは、同じヒープリストを使っていたのが問題そうでした。
3章ではこの問題のテストを作成し、問題をあぶり出した後、問題を解決しましょう。

まず、２つめの世界で計算した後に本来であれば2の世界はまだ生き延びていなくてはなりません。
そして、1の世界で新しいオブジェクトを作り、その後にもう一つ世界を作って、世界は終わらせずに、次のオブジェクトを作ってみましょう。

	< void test_multi_world() {
	<   enum {FRAME_START, FRAME_SIZE, A, B, C, D, FRAME_END};
	<   ENTER_FRAME_ENUM();
	<
	<   frame[A] = gc_alloc_int(1);
	<
	<   NEW_WORLD(frame_tmp1);
	<     frame[B] = test_int(frame[A]->intv);
	<   CLOSE_WORLD(frame_tmp1);
	<   frame[A] = gc_alloc_int(2);
	<   NEW_WORLD(frame_tmp2);
	<     frame[C] = test_int(frame[B]->intv);
	<   CLOSE_WORLD(frame_tmp2);
	<   frame[A] = gc_alloc_int(3);
	<   GET_WORLD_OBJECT(frame_tmp1, frame[B]);
	<   GET_WORLD_OBJECT(frame_tmp2, frame[C]);
	<
	<   printf("id change check.........\n");
	<   gc_collect();
	<   LEAVE_FRAME();
	< }

こんな感じか。こんな感じで、`CLOSE_WORLD`マクロと`GET_WORLD_OBJECT`マクロが欲しいのかな。
とりあえずこんな感じですよ。閉じた世界はまだ生きていて何か取り出す処理は遅延されています。
みたいなかんじですね。必要になるまで取り出されません。`GET_WORLD_OBJECT`で初めて取り出されますが、取り出した後もまた、生き残り続けます。というようなことを実現しなくてはなりません。

休憩しよう。そうしよう。

世界のハンドラを作ってそれを持って回る必要があります。閉じた世界は結果ではなくハンドルを持つのです。idでもよいのですが、GCによって解放されては困ります。しかしながら、誰も参照しなくなったのであれば解放されなくてはなりません。ペアか、レコードに保存すると良いんでしょう。レコードが速そうなのでレコードに保存して返す事にしましょう。

ファイナライザか、、、ファイナライザが必要です。

## 4. <a name="c4"></a>[ファイナライザ](#C4)

ファイナライザをレコードに登録し、消されるときにファイナライザが呼ばれる必要があります。コールバック変数を登録するリストを作り、コールバックが呼ばれるようにすれば、そして出来ればクロージャになっていれば関数型言語のバックエンドとして良く働いてくれるでしょう。SML#<a name="r1"></a>[[1]](#1)のランタイム<a name="r2"></a>[[2]](#2)で見た事があるぞ！確かこの辺だ<a name="r3"></a>[[3]](#3)。SML#では大きな数値計算のデータを消すためにファイナライザを使っていますね。

我々は我々のファイナライザ付きの、レコードを作りましょう。レコードのビットマップの手前に関数ポインタを入れておき、レコードの値を引数として渡せるようにすればよいでしょう。

GC実行時に解放されるオブジェクトがレコードで、コールバック関数がある場合は、自分を引数として呼び出しましょう。

これで、自動的に世界は消えてくれます。世界のレコードは、世界のヒープリストを持たせます。
そしてそのヒープリストをGCして解放すればよいのです。
もう、新しい言語を作って実装したい気持ちになってきますが我慢です。

データを取り出して解放という手段も提供したい所です。
しかしデータを取り出すだけで残す手段があっても良いかもしれません。
コピーが複数必要なら、残しておいても構わないからです。

オブジェクト思考のオブジェクトでも良いかもしれませんが、どうなんでしょうね。オブジェクトごとにメモリを割り当て、オブジェクトを作るとオブジェクト内で計算が行われる。オブジェクトごとに何か特別なIDが割り当てられて、メモリ空間を持ち、オブジェクトが消えるとメモリ空間も消える。そんなモデルが見えて来た気がします。RTTIなようだけど、参照カウンタではないし、なにかDOT計算<a name="r4"></a>[[4]](#4)的な感じもします。うーん。内部状態は変えてもいいんだよな。お約束としては。

オブジェクトごとに世界を作るには高コストな気もするしまぁ、必要な時だけ世界を作るのがいいよなと。場合によっては結果を複数持つ事も考えられそうだけど、話を膨らまし過ぎだ。Thread.startしてrunして終わったオブジェクトみたいなものですね。

で、何だっけ、データを取り出す方法が複数選択出来るようにするとよさそうなんですけど、そのためにはクロージャを使うか、オブジェクトのメソッドを使うと良さそうです。コンテキストとなるレコードが関数ポインタを持てばいいんですよね。

ここでは、計算結果を２つ出して、２つのデータを取り出せるテストプログラムを作りましょう。
そして、２つのデータをオブジェクトを生かした状態で取り出したり、取り出すと同時に解放してみたりしましょう。

要するにゴミのでない計算機が作れる。計算機から計算機を取り出せるとすると、計算機は複数の計算機で共有出来るものでなくてはならない。
共有する計算機のハンドルは、生のメモリヒープではなく、メモリヒープへのポインタでなくてはならない。

	- vmレコード
	// 	種類はOBJ_VM

	typedef struct VM {
	  ObjectHeader* heap_list; // ヒープデータ
	  void* record; // 内部状態
	} VM;

	- データ取得関数 void* vm_get_record(VM* vm)

		vmからレコードを取り出して返す
		この時のレコードのデータはvmのheap_list内に保存されていて、vmのidを持っているので、そのvmからvm_get_travarseを使ってidを変えて取り出す

	- gc_copy(Object* data)

		状態は残す必要があるのでgcのアルゴリズムを使わずコピーを取る。OBJ_VMの場合はシャローコピーする。

	- ファイナライザ void vm_finalize(void *vm) vmにはvmのポインタが入る。
		vm状態の解放処理をする。
		vm状態って何ですか？VM構造体をfreeして、heap_list内のデータを全部消すんです。
		heap_listをグローバルにコピってそれから、gcを呼び出す。

	他のデータ ---> VMレコード
	               ^
	               |
	他のデータ ------+

- ○ vmはいつ作る？
	初期化時や、NEW_WORLDを呼ばれたときに、vm_newで作ってみましょう。

- ○ vmのヘッダリストはいつ保存する？
	vmをnewしたときにしてみましょう。
	とりあえずやってみて駄目なら変えましょう。

とりあえず、関数は作った。世界を複数立ち上げてみよう。


	VM* vm1 = vm_new();
	VM* vm2 = vm_new();

ってすればできる。そしたら、vmを選択する。


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