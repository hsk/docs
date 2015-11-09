# マルチワールドGC

## もくじ

- <a name="C1"></a>[はじめに](#c1)
- <a name="C5"></a>[実装](#c5)
- <a name="C9"></a>[参考文献](#c9)

## <a name="c1"></a>[はじめに](#C1)

[new_world gc](../new_world) はレベルで世界を分けていました。
世界はネストしていて、階層構造にはなっていましたが、複数の世界が独立して存在する事はありませんでした。
ここでは、複数の世界を持つ事を考えます。

複数のメモリ領域があるということは、親の世界が子を複数持つ事が出来るという事です。
そして、子供同士がデータのやり取りが出来るかもしれません。
とにかく考えるべきは、IDを持ち、レベルではなくIDで管理する事でしょう。

計算世界のIDが同じであれば、同じ世界であり、IDが違う時は違う世界です。

## <a name="c5"></a>[実装](#C5)

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

    < #define NEW_WORLD(tmp,tmp_world) \
    <   int tmp_world = world_id;
    <   world_id = gen_world_id(); \
    <   Frame* tmp = frame_bottom;


    < #define END_WORLD(tmp,tmp_world,root) \
    <   gc_collect_end_world(root, tmp_world); \
    <   frame_bottom = tmp; \
    <   world_id = tmp_world;

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

メインにテストを追加します。

    <   printf("---\n");
    <   gc_init();
    <   test_multi_world();
    <   gc_free();

これで作業は完了です。

## <a name="c9"></a>[参考文献](#C9)