# Objective-CのARCに似たGCの方式の提案

## もくじ

1. <a name="C1"></a>[モチベーション](#c1)
2. <a name="C2"></a>[アイディア](#c2)
3. <a name="C3"></a>[アルゴリズム](#c3)
4. <a name="C5"></a>[実装](#c5)
5. <a name="C6"></a>[考察](#c6)
6. <a name="C7"></a>[今後の研究](#c7)
7. <a name="C4"></a>[関連研究](#c4)
8. <a name="C9"></a>[謝辞](#c9)
9. <a name="C8"></a>[参考文献](#c8)

ここでは、手動で世代管理をするガーベジコレクションの手法を提案します。

## 1. <a name="c1"></a>[モチベーション](#C1)

メモリ管理手法は様々な手法があります。
スタックを用いた変数のメモり管理は自動的にメモリが管理され使用が終わると自動的に解放されるため非常に高速で便利です。

ヒープを使ったメモリ管理は少し事情が異なります。
手動管理は高速に動作しますが開発が大変です。
参照カウンタ方式は、カウントのコストと循環参照の問題があります。
ガーベジコレクションは参照カウンタを更新する必要がありませんが、停止時間が長くなるのが頭の痛い問題でインクリメンタルGCをするための様々な研究があります。

Objective-CのAutoReleasePool<a name="r3"></a>[[3]](#3)は参照カウンタの管理コストを下げます。
AutoReleasePoolの仕組みをGCに応用し、マイナーGCを小さい空間で行う高速なアルゴリズムが出来ないでしょうか？
OCamlの多相的型推論はレベルを用いて解決している<a name="r6"></a>[[6]](#6)ので、その考えとAutoReleasePoolを組み合わせれば何か興味深いものが出来るかもしれません。

## 2. <a name="c2"></a>[アイディア](#C2)

ある、まとまった計算をする時、１つの世界を作り出し、そして計算が終わったらその世界から結果だけを取り出し世界を終わらせる。

このような漠然としたアイディアが生まれました。

複数パスに別れるような計算では、段階ごとに作成したデータを次のパスへ渡すポイントでコピーするようにすれば、奇麗なGCが行えるかもしれません。

プログラムの種類にもよりますが例えば、ゲームの場合は、フレームごとにキャラクターが移動します。
この場合、フレームごとに前の世代を参照しつつ新しいデータを更新して行けばGCを毎フレームごとに計算しながら行う事が出来そうです。

領域を分けるには、アドレスを使うのがよいかもしれません。
ヒープのサイズは64bitの時代なので1Gクラスに取れるようにしておけばほぼ問題なく動作させる事が出来るでしょう。
しかし、単純なリストだけでも美味くやればそれなりの物が作れるかもしれません。

おそらく２つのアルゴリズムが考えられそうです。

1. レベルで領域を管理する。
2. 領域を複数持ちIDを持って管理する。

二つの事を一度に考える事は出来ません。
単純そうな物から順番に考えて行きましょう。
そこで、今回はレベルで領域を管理するアルゴリズムを考えます。

## 3. <a name="c3"></a>[アルゴリズム](#C3)

我々のGCは、一つのまとまりの計算を始める時に、新しいヒープ空間をつくり、その空間の中で計算し終わったら空間を閉じます。
GCではSTOP THE WORLDというような言い方もするのでここではそのメモリ空間を世界と呼ぶ事にしましょう。
計算をするときに世界を作り、終わったら世界を閉じます。
スレッドに近いのですが、コンテキストとして作るのはメモリのヒープ空間だけです。

新しいコンテキスト上では外のレベルから受け取ったデータを参照は出来ますが変更はしない事にします。
新しい世界で作成したデータは外部から参照される事はありません。

新しい世界ではそのネストレベルを1上げ、終わったらレベルを1下げます。
世界が終った時には、計算結果をルート集合としてガーベジコレクションを行います。
ただし、マークやスイープを行うのは新しい世界のみです。
スイープ時には元の世界に移動します。元の世界には計算結果のみが増えるだけです。

余計なゴミは残りません。一部の世界の出来事のために世の中全体の大掃除は必要ないのです。
大風呂敷を広げれば、派手に散らかしても出来たもの以外を取り出したら後は風呂敷ごとまとめてポイすればいいのです。
習字をするときに新聞紙を広げるのと一緒です。図工をするには図工室に行くのが良いのです。畑からは収穫物だけを持ち帰れば良いのです。
農薬や害虫や泥は持ち帰る必要はありません。場合によってはさらに、肥料を作る場所が必要かもしれません。脱穀の作業も必要でしょう。その場その場で様々なゴミがでるかもしれませんがそのゴミはその場所で処理すれば良いのです。全てを持ち帰ったら大変な事になります。そりゃもうゴミだらけで後片付けは大変です。

話がそれてしまいました。アルゴリズムの話に戻りましょう。

[mark_and_sweep](../mark_and_sweep)を改造してリストを使って空間を分ける事を考えましょう。
新しい世界と、元の世界を区別するためにヘッダにレベルを持たせます。メモリ効率は悪いですが分かりやすさを優先して富豪的に行きましょう。
グローバル変数に現在のlevelを持ち、新しい世界に入ったらlevelをカウントアップし、終わったらカウントダウンします。
メモリアロケーション関数は呼び出し時のlevelをオブジェクトのヘッダにコピーします。

新しい世界でも場合によってはメモリが足りなくなりGCが発生します。
このときマークフェーズではスタックフレームをルート集合としてマークを行いますが、
新しい世界のマークはコンテキストを切り替えた時点のスタックフレームまでにして、親の世界までは影響を及ぼしません。
スイープフェーズでは、ヒープのリストを辿り、現在レベルのオブジェクトのみを解放します。

計算が終わったら、世界を切り替えるための特別なGCを行います。
フレームはルートに加えずに、計算結果だけをルート集合としてマークします。
またスイープ時に残ったオブジェクトはレベル１つ下げることで世界を移動します。
こうすることで計算結果のみが元の世界に残ります。最後にグローバルのレベルを元に戻して終わります。

動作のイメージを書いてみましょう。世界の始まりでは`heap_list`は空です。

                level = 1
                heap_list -> null

計算が進みメモリ割り当てが進むと、`heap_list`に値が増えて行きます。

                level = 1
                heap_list -> (l1 4)->(l1 3)->(l1 2)->(l1 1)->null

新しい世界を作り、新しい世界に入ると、レベルが上がります。

                level = 2
                heap_list -> (l1 4)->(l1 3)->(l1 2)->(l1 1)->null

新しい世界で計算します。

                level = 2
                heap_list -> (l2 5)->(l2 6)->(l2 7)->
                               | |           ^
                               | |           |
                               | +-----------+
                               v
                -> (l1 4)->(l1 3)->(l1 2)->(l1 1)->null

計算が終わったので5の値を返したいので、5をルートとしてマークします。
このとき、レベルが5のレベルより小さいポインタは親の世界なのでマークしません。下の図では`*`がマークを表しています。

                level = 2
                heap_list -> (l2*5)->(l2 6)->(l2*7)->
                               | |           ^
                               | |           |
                               | +-----------+
                               v
                -> (l1 4)->(l1 3)->(l1 2)->(l1 1)->null

マークが終わったら、世界を終わらせる特別なスイープをします。と言っても現状のレベルを1つ下げるだけです。
レベルが高いものだけマークされている物を残して現状レベルに下げ、マークされていない物は解放します。

                level = 1
                heap_list -> (l1 5)  (l2x6)  (l1 7)->
                               | |           ^
                               | |           |
                               | +-----------+
                               v
                -> (l1 4)->(l1 3)->(l1 2)->(l1 1)->null

最終的には以下のように無駄のない状況になります。

                level = 1
                heap_list -> (l1 5)->(l1 7)->(l1 4)->(l1 3)->(l1 2)->(l1 1)->null
                               | |   ^               ^
                               | +---+               |
                               +---------------------+

イメージ出来たでしょうか？

## 4. <a name="c5"></a>[実装](#C5)

それでは実装に移りましょう。[gc.c](gc.c)に実装があります。ここでは、[mark_and_sweep](../mark_and_sweep)との差分を見て行きましょう。

ObjectHeaderの構造体にレベルを持たせます。

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

スイープの関数には、世界が終わった時のためにレベルを渡すようにします。

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

    < void gc_collect_end_world(Object* data) {
    <   int prev_num = heap_num;
    <   gc_mark_object(data);
    <   gc_sweep(heap_level-1);
    <
    <   heap_max = prev_num * 2;
    <
    <   debug("Collected %d objects, %d remaining.\n", prev_num - heap_num,
    <          heap_num);
    < }

世界を開始するマクロを追加して使います。`tmp`にはバックアップを取るフレーム名を指定します。

    < #define NEW_WORLD(tmp) \
    <   heap_level++; \
    <   Frame* tmp = frame_bottom;


    < #define END_WORLD(tmp,root) \
    <   gc_collect_end_world(root); \
    <   frame_bottom = tmp; \
    <   heap_level--;

通常のスイープは引数を`0`で呼び出します。

    <   gc_sweep(0);
    ---
    >   gc_sweep();

オブジェクトを作成した場合はレベルを保存します。

    <   head->level = heap_level;

初期化時はヒープレベルを1に、フレームボトムは`NULL`にします。

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

これで作業は完了です。

## 5. <a name="c6"></a>[考察](#C6)

AからB、BからC、CからDへとデータを受け渡すような場合を考えます。
一度世界を作ってから、さらにA,B,Cの世界をつくって呼び出しましょう。

    O
      a<---A
      b<---B
      c<---C
    GCが3回
    a,b,cが残る。

- Aを使った後に受け取ったデータだけが世界に残ります。
- Bに渡され、AとBが作ったデータが残ります。
- Cを呼び出すと、AとBとCの作ったデータだけが残ります。

Aの作ったデータはもはやいらないはずですが残ってしまいます。
別に残ったって良いじゃないかといわれればそうなのですが、消す事を考えてみましょう。

最初の呼び出しを2つの世界を一気に作って以下のようなイメージで繋げてみたらどうでしょう。

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
        a special gc
        |
        v
        B
        |
        b special gc
        |
        v
    c<--C

これも簡単に出来ます。Aから受け取ったデータをルート集合として、マイナーなGCをして世界は終わらせずにBの処理をするのです。
これは、テストコードとして実装出来そうです。

以下のコードは、空間上に3つのデータが残っていて最後にまとめて解放します。

    void test_pipes1() {
      enum {FRAME_START, FRAME_SIZE, A, B, C, FRAME_END};
      ENTER_FRAME_ENUM();

      frame[A] = gc_alloc_int(1);

      NEW_WORLD(frame_tmp1);

        frame[B] = test_new_world2(frame[A]);
        frame[B] = test_new_world2(frame[B]);
        frame[B] = test_new_world2(frame[B]);

      printf("level change check.........\n");
      END_WORLD(frame_tmp1,frame[B]);
      printf("level change check.........\n");
      gc_collect();
      LEAVE_FRAME();
    }

この中の世界の中でgc_pipeを呼びます。

        frame[B] = test_new_world2(frame[A]);
        gc_collect_pipe(frame[B]);
        frame[B] = test_new_world2(frame[B]);
        gc_collect_pipe(frame[B]);
        frame[B] = test_new_world2(frame[B]);

計算途中のデータは`gc_collect_pipe`を用いる事で解放できます。

## 6. <a name="c7"></a> [今後の研究](#C7)

ここでは世界を分ける方法としてリストを使った手法のみを考えました。
ビットマップを使った場合も考えると良いでしょう。

外の世界に書き込む場合はライトバリアなどを使って、ルート集合に追加する必要があるでしょうがここでは触れませんでした。
また、ビットマップGC等を使ってアドレスで領域を分ければレベルデータを保存する必要は無くなるでしょう。
コピーのコストはバカにできないので、データが小さい場合にはコピーを行い大きいデータはレベルで管理するのがよいでしょう。
リスト構造ではない、ハッシュマップ構造等にして高速化をした場合はレベル別のリストを別に容易する必要があるかもしれません。
何にせよ様々な手法でよりよいアルゴリズムを使うと良いでしょう。

この機能を実装した言語を開発する事で、高速なGCの仕組みを持った言語を作れるはずです。

## 7. <a name="c4"></a>[関連研究](#C4)

世代別GCは短命の若い世代のオブジェクトと長命のオブジェクトを分け、若い世代だけのGCを行うマイナーGCとメジャーGCに分けてGCを行います。マイナーGCではルート集合に古い世代のオブジェクトからの参照を含めます。

Erlangのようなマルチプロセスな<a name="r9"></a>[[9]](#9)は複数の軽量なグリーンプロセスを立ち上げアクター毎にメモリ管理を行うことが出来ます。プロセス同士は他のメモリ領域を見る事が出来ません。

Golangのgoルーチン<a name="r8"></a>[[8]](#8)や、Rustのspawn<a name="r7"></a>[[7]](#7)は並列化のためにメモリ空間を分けて計算する事が可能です。

ATSの線形型<a name="r1"></a>[[1]](#1)や、Rustのオーナーシップ<a name="r2"></a>[[2]](#2)は型によってリソースの解放を保証します。

OStack Lua<a name="r10"></a>[[10]](#10)はLuaのメモリをスタック上に取ることで高速に実行する実装です。\[Erick06\]<a name="r11"></a>[[11]](#11)の手法をLuaに実装したもので、元々ヒープから取得するデータをスタック上に割り当てることでキャッシュが荒らされずに高速に動作します。

プロセス呼び出しをパイプで繋ぎ、データをやり取りすれば、プロセス単位でメモリは解放されるのでGCをそもそも使う必要がないかもしれません。

## 8. <a name="c9"></a> [謝辞](#C9)

copy GCのことはすっかり頭から抜けていたので、微酔半壊: copying GCに対する改良[[4]](#4)および微酔半壊: Copying Garbage Collector[[5]](#5)は参考になりました。
また何となくtwitterで多くの人に励まされていたように思います。RubyのGCの話や、Bitmap GCをpicrinに入れよう等と言う話がなければ思いつく事はありませんでした。
感謝です。

## 9. <a name="c8"></a> [参考文献](#C8)

- <a name="1"></a>[[1]](#r1) AST2 線形型

    http://ats-lang.sourceforge.net/DOCUMENT/INT2PROGINATS/HTML/c4154.html#simple-linear-objects

- <a name="2"></a>[[2]](#r2) Rust オーナーシップ

    https://doc.rust-lang.org/book/ownership.html

- <a name="3"></a>[[3]](#r3) Objective-C ARCによるメモリ管理 Saturday, December 31st, 2011

    http://cx5software.sakura.ne.jp/blog/2011/12/31/objective-c-memory_management_by_arc/

- <a name="4"></a>[[4]](#r4) 微酔半壊: copying GCに対する改良 2007年04月01日

    http://smpl.seesaa.net/article/37446952.html

- <a name="5"></a>[[5]](#r5) 微酔半壊: Copying Garbage Collector 2007年03月17日

    http://smpl.seesaa.net/article/36160135.html

- <a name="6"></a>[[6]](#r6) How OCaml type checker works -- or what polymorphism and garbage collection have in common

    http://okmij.org/ftp/ML/generalization.html

- <a name="7"></a>[[7]](#r7) Rust thread

    https://doc.rust-lang.org/std/thread/

- <a name="8"></a>[[8]](#r8) Gorutine

    https://tour.golang.org/concurrency/1

- <a name="9"></a>[[9]](#r9) BEAM(Erlang VM) 参考資料まとめ

    http://blog.etsukata.com/2014/01/erlang-beam.html

- <a name="10"></a>[[10]](#r10) OStack Lua (github)

    https://github.com/hogelog/ostacklua

- <a name="11"></a>[[11]](#r11) [Erik06] Erik Corry. Optimistic stack allocation for Java-like languages. In J. Eliot B.
Moss,editor, ISMM'06 Proceedings of the Fourth International Symposium on
MemoryManagement, pp. 162―173, Ottawa, Canada, June 2006. ACM Press.

    http://www.researchgate.net/publication/221032948_Optimistic_stack_allocation_for_Java-like_languages

    http://www.cs.technion.ac.il/~erez/ismm06/slides/foredrag.pdf.pdf
