# Objective-CのARCに似たGCの方式の提案

## もくじ

1. <a name="C1"></a>[モチベーション](#c1)
2. <a name="C2"></a>[アイディア](#c2)
3. <a name="C3"></a>[アルゴリズム](#c3)
4. <a name="C4"></a>[他のアルゴリズムとの違い](#c4)
5. <a name="C5"></a>[実装](#c5)
6. <a name="C6"></a>[今後](#c6)
7. <a name="C7"></a>[参考文献](#c7)

ここでは、手動での世代管理をするガーベジコレクションの手法を提案します。

## 1. <a name="c1"></a>[モチベーション](#C1)

メモリ管理手法としては、手動管理が速いですが開発が大変です。
参照カウンタ方式は、カウントのコストが気になります。循環参照を検査する事で解放は可能ですが、循環参照の問題もあります。
ガーベジコレクションはメモリ空間が広がるとGC時間が肥大します。
プロセス呼び出しをパイプで繋ぎ、データをやり取りすれば、プロセス単位でメモリは解放されるので、保守的なGCでも十分でしょうし、GCをそもそも使う必要がないかもしれませんが、プロセスの起動コストとデータのシリアライズとデシリアライズのコストが気になります。
ErlangのようなマルチVMのアクターモデルは、素晴しいですがメッセージのやり取りをする必要はありません。
Objective-CのAutoReleasePoolは面白いアイディアですが、参照カウンタ方式です。[[3]](#3)
ATSの線形型[[1]](#1)や、Rustのオーナーシップ[[2]](#2)は型によってリソースの解放を保証しますが、型を意識して書くプログラムは手間がかかります。
Golangのgoルーチン[[8]](#8)や、Rustのspwawn[[7]](#7)はメモリ空間を分けますが並列化の機能です。並列化にはスレッド起動かアクターのようなやり取りが必要で高コストです。
OCamlの多相的型推論はレベルを用いて解決していますが、その考えをオートリリースプールに応用出来るかもしれません。
なんとか、マイナーGCを小さい空間で行う高速なアルゴリズムが作れない物かと思う訳です。

## 2. <a name="c2"></a>[アイディア](#C2)

新しいメモリ空間のみを作り計算し、終わったら計算結果をルート集合としてガーベジコレクションを行うようなことが出来ればいいのではないかという漠然とした考えが生まれました。

参照は出来るが、結果のコピーが必要な場合は、関数を通して取得します。

プログラムの種類にもよりますが例えば、ゲームの場合は、フレームごとにキャラクターが移動します。
この場合、フレームごとに前の世代を参照しつつ新しいデータを更新して行けばGCを毎フレームごとに計算しながら行う事が出来そうです。

複数パスに別れるような計算では、段階ごとに作成したデータを次のパスへ渡すポイントでコピーするようにすれば、奇麗なGCが行えそうです。
ヒープのサイズは64bitの時代なので1Gクラスに取れるようにして置けばほぼ問題なく動作させる事が出来る場合が大半でしょう。
しかし、単純なリストだけでも美味くやればそれなりの物が作れるかもしれません。

というような漠然としたアイディアを実装するアルゴリズムを考えてみようと思います。

おそらく２つのアルゴリズムが考えられそうです。

1. レベルで領域を管理する。
2. 領域を複数持ちIDを持って管理する。

今回はまず1のレベルで領域を管理するアルゴリズムを提案します。とはいっても、他で既にありそうですけど。

## 3. <a name="c3"></a>[アルゴリズム](#C3)

新しい計算モデルでは、計算空間を作成する関数を作りここで、新しいヒープ空間をつくります。

スレッドに近いのですが、ヒープを別空間に作ります。

ヒープ空間を作ると、そこで作成するオブジェクトは別空間で生成されます。

ヒープ空間を使った計算が終わったら、計算結果をルート集合としてガーベジコレクションを行います。
ただし、検索するのは新しい空間のみとします。

これにより、返却されるデータは全て、元の空間にコピーされますが、元のデータ空間は最小限の変更しかないのでマイナーGCが実行されるのに等しくなります。

新しい領域は、データにレベルを持たせる事で作成します。グローバル変数にlevelを持ち、新しい領域に入ったらカウントアップします。
メモリアロケーション関数はlevelをオブジェクトに持たせます。

GCが起きた場合は、levelを見て、現在レベルより低いレベルのオブジェクトまでは見に行きません。
また、スイープの段階でも、現在レベルのみ解放します。
新しい計算領域では、元のレベルから受け取ったデータを参照は出来ますが変更はできません。
なので、外から参照される事はありません。

計算が終わったら、計算結果をルート集合としてマークします。スイープし、残ったオブジェクトはレベルを下げます。
こうすると結果のみが残ります。最後にグローバルのレベルを元に戻して終わります。

[mark_and_sweep](../mark_and_sweep)に新たに、ヒープを作れる用にするだけです。

heap_listはから

                level = 1
                heap_list -> null
                world_list -> null

メモリ割り当てが進む

                level = 1
                heap_list -> (l1 4)->(l1 3)->(l1 2)->(l1 1)->null
                world_list -> null

新しい領域に入ると、新しい領域に入った時のリストのトップを保存する。

                level = 2
                heap_list -> (l1 4)->(l1 3)->(l1 2)->(l1 1)->null

新しい領域で計算する。

                level = 2
                heap_list -> (l2 5)->(l2 6)->(l2 7)->
                               | |           ^
                               | |           |
                               | +-----------+
                               v
                -> (l1 4)->(l1 3)->(l1 2)->(l1 1)->null


計算が終わったので5の値を返したいので、5をルートとしてマークする。
このとき、レベルが5のレベルより小さいポインタは親領域なのでマークしない。

                level = 2
                heap_list -> (l2*5)->(l2 6)->(l2*7)->
                               | |           ^
                               | |           |
                               | +-----------+
                               v
                -> (l1 4)->(l1 3)->(l1 2)->(l1 1)->null

マークが終わったら、スイープをする。このときのスイープは現状のレベルを1つ下げる。
レベルが高いものだけマークされている物を残して現状レベルに下げ、マークされていない物は消す。

                level = 1
                heap_list -> (l1 5)  (l2x6)  (l1 7)->
                               | |           ^
                               | |           |
                               | +-----------+
                               v
                -> (l1 4)->(l1 3)->(l1 2)->(l1 1)->null



## 4. <a name="c4"></a>[他のアルゴリズムとの違い](#C4)

通常の世代別GCとの違いは、GCの世代交代のタイミングを自由に選べる事です。
特に重要なポイントでエリアを分ける事で高速なGCが可能になります。
参照ポインタ方式ではカウントの上げ下げが面倒でしたが、カウントの上げ下げは必要ありません。
コンテキストの切り替えコストは最小で、レベルの書き換えだけで済むのでコピー操作もありません。

## 5. <a name="c5"></a>[実装](#C5)

gc.cに実装があります。

オブジェクトヘッダにレベルを持たせます。

    <   unsigned int level;

グローバル変数に`heap_level`と`frame_bottom`を追加します。

    < int heap_level;
    < Frame* frame_bottom;

マーク時にヒープレベルより小さければマークしないようにします。

    <   if (head->marked || head->level < heap_level) return;
    ---
    >   if (head->marked) return;

マークするルート集合は新しく作り出したフレームだけにします。

    <   while(frame != frame_bottom) {
    ---
    >   while(frame) {

スイープの関数には、世界が終わったときにレベルを渡すようにします。

    < void gc_sweep(int level) {
    ---
    > void gc_sweep() {

スイープのループ中にレベルがヒープのレベルよりも低くなれば終了します。

    <     if((*object)->level < heap_level) break;

レベル指定があればレベルを元に戻します。

    <       if(level) {
    <         printf("level change\n");
    <         printf("level change %d -> %d\n", (*object)->level, level);
    <         (*object)->level = level;
    <       }

世界が終わった時の特別なGCを作ります。

    < void gc_minor(Object* data) {
    <   int prev_num = heap_num;
    <   gc_mark_object(data);
    <   gc_sweep(heap_level-1);
    <
    <   heap_max = prev_num * 2;
    <
    <   debug("Collected %d objects, %d remaining.\n", prev_num - heap_num,
    <          heap_num);
    < }

世界を開始するマクロを追加して使います。tmpにはバックアップを取るフレーム名を指定します。

    < #define NEW_WORLD(tmp) \
    <   heap_level++; \
    <   Frame* tmp = frame_bottom;


    < #define END_WORLD(tmp,root) \
    <   gc_minor(root); \
    <   frame_bottom = tmp; \
    <   heap_level--;

通常のスイープは引数を0で呼び出します。

    <   gc_sweep(0);
    ---
    >   gc_sweep();

オブジェクトを作成した場合はレベルを保存します。

    <   head->level = heap_level;

初期化時はヒープレベルを1に、フレームボトムはNULLにします。

    <   heap_level = 1;
    <   frame_bottom = NULL;

テストコードを追加します。

    < Object* test_new_world2(Object* data) {
    <   enum {FRAME_START, FRAME_SIZE, A, B, C, FRAME_END};
    <   ENTER_FRAME_ENUM();
    <
    <   // レコード
    <   enum {RECORD_SIZE=3,RECORD_BITMAP=BIT(1)|BIT(2)};
    <   frame[A] = gc_alloc_record(RECORD_SIZE); // 4
    <   frame[A]->longs[0] = 100; // undata
    <   frame[A]->field[1] = gc_alloc_int(200); // 5
    <   frame[A]->field[2] = data;
    <   frame[A]->longs[RECORD_SIZE] = RECORD_BITMAP;// レコードのビットマップ(cpuビット数分でアラインする。ビットマップもcpu bit数)
    <
    <   frame[B] = gc_alloc_int(3); // 6
    <   frame[C] = gc_alloc_int(5); // 7
    <   gc_collect();
    <   gc_collect();
    <
    <   LEAVE_FRAME();
    <   return frame[A];
    < }
    <
    < void test_new_world() {
    <   enum {FRAME_START, FRAME_SIZE, A, B, FRAME_END};
    <   ENTER_FRAME_ENUM();
    <
    <   // レコード
    <   enum {RECORD_SIZE=3,RECORD_BITMAP=BIT(1)|BIT(2)};
    <   frame[A] = gc_alloc_record(RECORD_SIZE); // 1
    <   frame[A]->longs[0] = 10; // undata
    <   frame[A]->field[1] = gc_alloc_int(20); // 2
    <   frame[A]->field[2] = test_int(30); // 3
    <   frame[A]->longs[RECORD_SIZE] = RECORD_BITMAP;// レコードのビットマップ(cpuビット数分でアラインする。ビットマップもcpu bit数)
    <
    <   NEW_WORLD(frame_tmp1);
    <
    <     NEW_WORLD(frame_tmp2);
    <     frame[B] = test_new_world2(frame[A]);
    <     END_WORLD(frame_tmp2, frame[B]);// 6と7が消える。
    <
    <   printf("level change check.........\n");
    <   END_WORLD(frame_tmp1,frame[B]);// 6と7が消える。
    <   printf("level change check.........\n");
    <   gc_collect();
    <   LEAVE_FRAME();
    < }

メインにテストを追加します。

    <   printf("---\n");
    <   gc_init();
    <   test_new_world();
    <   gc_free();

## 6. <a name="c6"></a>[今後](#C6)

AからB、BからC、CからDへとデータを受け渡すような場合を考えると、一度世界を作ってから、さらにA,B,Cの世界をつくって呼び出すと、Aを使った後に受け取ったデータだけが世界に残り、そのあとBに渡され、AとBが作ったデータが残り、Cを呼び出すと、AとBとCの作ったデータだけが残ります。Aの作ったデータはもはやいらないはずですが残ります。


    O
      a<---A
      b<---B
      c<---C
    GCが3回
    a,b,cが残る。

最初の呼び出しを2つの世界を一気に作って

    O1
        O2
            a<--A
        b<--b<--B
        O2
    c<--c<--c<--C

    cだけが残るが、GCの回数が６回。必要以上に多い。

この回数が多いとそれだけ遅くなります。コピーを3回で済ませたい所です。

    O1
        O2
            a<--A
        b<------B
        O2
    c<----------C

子から子へのデータを受け渡すような処理なのであれば、世界を終わらせずにGCを行えるとより効率的でしょう。
世界は１つのデータ領域を作り、受け渡しのデータをルート集合としてGCをしたら、次の処理を始めるのです。

    O1
        A
        |
        v
        B
        |
        v
    c<--C

これも簡単に出来ます。Aから受け取ったデータをルート集合として、マイナーなGCをして世界は終わらせずにBの処理をするのです。


学術的な事は良くわからないのですが、サーベイを確実に行い提案されていないのであれば論文としてまとめると良いのかもしれません。
外の世界に書き込む場合はライトバリアなどを使って、ルート集合に追加する必要があるでしょうがここでは触れません。
また、ビットマップGC等を使ってアドレスで領域を分ければレベルデータを保存する必要は無くなるでしょう。
しかし、コピーは必要になります。データが小さい場合にはコピーを行い大きいデータはレベルで管理するのがよいかもしれません。
リスト構造ではない、ハッシュマップ構造等にして高速化をした場合はレベル別のリストを別に容易する必要があるかもしれません。
何にせよ様々な手法でよりよいアルゴリズムを使うと良いでしょう。



## 7. <a name="c7"></a> [参考文献](#C7)

- <a name="1"></a>[[1]](r1) AST2 線形型

    http://ats-lang.sourceforge.net/DOCUMENT/INT2PROGINATS/HTML/c4154.html#simple-linear-objects

- <a name="2"></a>[[2]](r2) Rust オーナーシップ

    https://doc.rust-lang.org/book/ownership.html

- <a name="3"></a>[[3]](r3) Objective-C ARCによるメモリ管理 Saturday, December 31st, 2011

    http://cx5software.sakura.ne.jp/blog/2011/12/31/objective-c-memory_management_by_arc/

- <a name="4"></a>[[4]](r4) 微酔半壊: copying GCに対する改良 2007年04月01日

    http://smpl.seesaa.net/article/37446952.html

- <a name="5"></a>[[5]](r5) 微酔半壊: Copying Garbage Collector 2007年03月17日

    http://smpl.seesaa.net/article/36160135.html

- <a name="6"></a>[[6]](r6) How OCaml type checker works -- or what polymorphism and garbage collection have in common

    http://okmij.org/ftp/ML/generalization.html

- <a name="7"></a>[[7]](r7) Rust thread

    https://doc.rust-lang.org/std/thread/

- <a name="8"></a>[[8]](r8) Gorutine

    https://tour.golang.org/concurrency/1
