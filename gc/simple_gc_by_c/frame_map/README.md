# Cのみで作るスタックマップ付きGC

1. はじめに
2. フレームマップ
3. スタックトレース
4. フレームマップ登録の仕組み
5. フレームマップ付きGC
6. より詳細なビットマップ付きフレームマップ付きGC
7. まとめ

## 1. はじめに

LLVMのスタックマップ付きのGCをCだけで作成します。

シャドースタックを使った単純なGCの実装では、実行時にマネージされたポインタ用の領域のリストの更新が必要でした。
ここでは、実行時のシャドースタックの更新をなくし、代わりに実行アドレスからフレームのアドレスを取得する仕組みを作ります。
要するに実行アドレスからのフレームへのマップ、フレームマップを予めコンパイル時に保存しておく事で、高速化します。


２章でフレームマップについて説明し、３章ではスタックトレースにより実行アドレスを取得する方法を説明し実装します。
４章でフレームマップの登録の仕組みをC言語で作成し、５章では単純なフレームマップ付きGCをC言語で作成します。
６章ではより詳細なフレームマップをビットマップを使って実装します。

## 2. フレームマップ

この章ではフレームマップに付いて説明します。
フレームマップとは実行アドレスから、フレームへの写像です。

実行中のプログラムのスタックフレームを辿り、リターンアドレスからフレームを検索できるようにすれば、実行時にフレームリストの更新は必要ないのです。

例として以下のような関数を考えましょう:

	fun f1(){
		j = 3;
		gc();
	l1a:
	}

	fun f2(){
		i = 1;
		a = [ 1 ];
		f1();
	l2a:
		j = 2;
		b = [a[0]];
		f1();
	l2b:
		return b;
	}

f1ではgcを呼び、f2では配列を作って、複数回f1を呼び出しています。

GCが呼ばれたときにスタックトレースして変数が入っているフレームを取得しましょう。
f2を呼ばれるとき、スタックには次のアドレスが戻りのアドレスとして保存されます。
スタックの状態はgcが呼ばれるまで以下のように積まれて行く事でしょう。

	stack i
	stack i,a
	stack i,a,l2a

最初にf2が呼び出されると、f2の変数iが積まれ、aの配列が積まれ、f1を呼び出すと、戻りのアドレスl2aが積まれます。

	stack i,a,l2a,j
	stack i,a,l2a,j,l1a

f1ではjが詰まれ、gcの呼び出しでl1aが積まれます。

GCはこのスタックを見て、リターンアドレスのl1aからjのアドレスをフレームマップから発見します。
次にl2aのアドレスからフレームマップからiのアドレスを見つけるのです。


また、マルチスレッドでのGCの場合も、実行中のスレッドを止めてみる必要があります。
この場合もスレッドの実行アドレスとスタックから発見する事が出来ます。

このようなことを実現するのがフレームマップというわけです。

次の章ではスタックトレースを見る方法について説明します。

## 3. スタックトレースとフレームアドレス

フレームマップを使ったGCではスタックトレースをする必要があります。
そこでこの章ではスタックトレースの取り方に付いて説明します。

C言語は様々なプラットフォームで動作するプログラミング言語です。
スタックはマシン語とに異なり、スタックの使い方は実装に依存します。
ここでは、我々が使用しているMac IntelのOSX上GCCを用いて、x86_64の環境でスタックトレースする事を考えます。

	Machine Stack

           |<-  64bit  ->| (x86_64)
	 sp?-> |             |
	              :
	       |             |
	    -> | f0 data[0]  | <-- &data
	              :
	 bp?-> | return addr | ---+ <-- &(&data)[1] (x86)
	    -> |        (f1) |    |
	              :
	       | f1 data2    |    |
	       | f1 data2    |    |
	       | f1 data2    |    |
	       | f1 data[0]  |    |
	       | f1 data[1]  |    |
	       | f1 data[2]  |    |
	              :
	  +--- | return addr | <--+
	  |    |        (f2) |
	              :
	  |    |             |
	  |    | f2 data[0]  | <-- &data[0]
	  |    | f2 data[1]  | <-- &data[1]
	  |    | f2 data[2]  | <-- &data[2]
	              :
	  +--> | return addr | ---+
	       |        (f3) |    |
	              :
	       | f3 data[0]  |    |
	       | f3 data[1]  |    |
	       | f3 data[2]  |    |
	       | f3 data[3]  |    |
	       | f3 data[4]  |    |
	              :
	  +--- | return addr | <--+
	  |    |      (main) |
	              :
	  |    |             |
	  +--> | return addr | --+ 
	       | (c runtime) |   |


x86_64では、スタックは上位アドレスから下位アドレスへと成長します。
スタックは64bit単位で構成されています。
スタック用のレジスタはrspとベースポインタ用のrbpがあり、rbpからローカル変数を参照します。
リターンアドレスをスタックから見つけて辿って行くとスタックをトレースする事が出来ます。
ローカル変数のポインタのスタックのアドレスを知る事が出来るので、そこから辿っていけばよいわけです。
ただこのリターンアドレスはコンパイラの実装によって変数の位置は変わるのですね。

test3.cのようなプログラムを書いて、
スタックのアドレスが分かればその位置から前後してリターンアドレスが見つかります。

スタックの中には変数のデータや戻り値以外にも様々な情報が書き込まれています。
その中で、GCをする際に欲しい情報は、ポインタが格納されている配列のアドレスとサイズです。

x86_64の場合、リターンアドレスから２つ前に最初に宣言した配列が配置されていました。
フレームを入れるための配列を用意するにはサイズが必要ですのでそれも調べてみました。

パディングされるのでlongを使うとcpuのポインタのサイズをえられるのでlongを使います。
longの配列のサイズは2以上にすると、x86でもトップがえられます。
ただし、これは実装依存ですので、ここで重い通りに動く事が重要です。

## 4. Cでフレームマップを保存する

スタックトレースは出来ました。この章ではCでフレームマップを保存する方法を考え実装します。


gccやclang等ではラベルのアドレスを&&で取得する事が出来ます。(VCでは出来ないようなのですが)
実験的なものをつくるならgccで十分でしょう。
しかし、外に持ち出す事は出来ません。
ここではスタックマップの仕組みを分かりやすくしたいのであって、速度は重視していません。
従って、最初の実行時にスタックマップを登録する事にします。
&&を使うので、実行時の初期化チェックは、ifを使わずにポインタへのgotoを使いましょう。

フレームマップは、フレーム構造体をリストにして保存します。
実行時に分からないのは、オブジェクトフレームのサイズだけです。
関数のアドレスからのリストを作っても良いのですが、より詳細な情報を保存するために、関数の終了位置のラベルも保存してみましょう。

test4.cで見る事が出来ます。

登録関数は作成してませんが、スタックマップを作る仕組みはできました。


	typedef struct FrameMap {
	  unsigned short frame_size;
	  void* start;
	  void* end;
	  struct FrameMap* next;
	} FrameMap;

`gc_add_frame_map`関数では`gc_frame_map_list`へFrameMapを登録します。

	FrameMap* gc_frame_map_list;

	void gc_add_frame_map(FrameMap* frame_map) {
	  frame_map->next = gc_frame_map_list;
	  gc_frame_map_list = frame_map;
	}

`gc_top_ptr`がスタックのトップを保持します。

	void** gc_top_ptr;

gc初期化時には、スタックトップを保存するようにします。

	void gc_init() {
	  void* data;
	  gc_top_ptr = (&data+1)[0];
	  heap_list = NULL;
	  heap_num = 0;
	  heap_max = 8;
	}

# 5. GCを作成する

それでは、gcを作成してみましょう。
4章で作成したスタックマップを保存する仕組みを[mark_and_sweep](../mark_and_sweep)のgc.cへ組み合わせます。


gc時には、スタックのリストを辿る代わりに、スタックのポインタから辿り、stack\_map\_listを検索し、関数を見つけ出します。
関数が見つかったらビットマップを探し、そのビットマップを元に、フレーム情報のみをマークします。

test5.cがそれです。test5.cはx86_64でのみ動作を確認しました。

gcコレクト時には、スタックのトップを取得し渡します。

	void gc_collect() {
	  void* data;
	  int prev_num = heap_num;
	  //printf("gc %p ptr %p\n", &data+1, ptr);// スタックトップ

	  gc_mark((void**)(&data+1), NULL);
	  gc_sweep();

	  heap_max = prev_num * 2;

	  debug("Collected %d objects, %d remaining.\n", prev_num - heap_num,
	         heap_num);
	}

マーク関数は、

	void gc_mark(void** ptr, void* addr) {
	  do {

リターンアドレスを求め、呼び出し元のポインタを取得し

	    addr = ptr[1];
	    ptr = (void**)(ptr[0]);

`gc_mark_find_frame_map`アドレスからフレームマップを取得し、なければ次に進みます。

	    FrameMap* frame_map = gc_mark_find_frame_map(addr);
	    if (!frame_map) continue;

フレームマップのサイズからフレームを取得し`gc_mark_frame_map`でマークします。

	    Object** objects = (Object**)&ptr[-1 - frame_map->frame_size];

	    gc_mark_frame_map(frame_map->frame_size, objects);

これをスタックトップまで続けます。

	  } while(ptr < gc_top_ptr);
	}


アドレスからフレームを検索する関数はフレームマップのリストを線形で範囲で検索するだけです。

	FrameMap* gc_mark_find_frame_map(void* addr) {
	  FrameMap* frame_map = gc_frame_map_list;
	  while (frame_map) {
	    if (frame_map->start <= addr && addr <= frame_map->end)
	      return frame_map;      
	    frame_map = frame_map->next;
	  }
	  return NULL;
	}

フレームのマークはフレームのサイズ分ループしてマークします。

	void gc_mark_frame_map(int size, Object** objects) {
	  for(int i = 0; i < size; i++)
	    gc_mark_object(objects[i]);
	}

# 6. より詳細なフレームマップ

5章では単純なフレームマップを作ったGCでしたが、大きなフレームマップがある場合は既に死んでいるオブジェクトを検索する可能性があります。そこで、より細かくラベルを付け、フレーム中で仕様中の物だけをリストアップする事を考えます。
要するに、ラベルと使用しているかしていないかを示すビットマップの配列が必要です。ビットマップの配列のサイズはフレームのサイズから求まりますが、最初はほとんど使用していないはずなので、ビットマップのサイズも持つ事にします。
コンパイラが美味くコンパクションして最初のほうのアドレスだけを使ってくれれば、より検索が速くなり高速に動作するはずです。

test5.cを拡張したのがtest6.cです。

Frame構造体を新たに追加します。

	typedef struct Frame {
	  void* lbl;
	  unsigned short bitmap_size;
	  int* bitmap;
	} Frame;

FrameMap構造体には新たに、Frame*を格納する領域を追加します。フレームのサイズも持ちます。

	typedef struct FrameMap {
	  unsigned short frame_size;
	  void* start;
	  void* end;
	  Frame* frames;
	  struct FrameMap* next;
	} FrameMap;

たとえば、test関数では、以下のようにしてより詳細なフレームマップを保存します。

	void test() {
	  void* frame[1];

	  static void* start_ptr = &&end; goto *start_ptr; start:;

	  printf("frame[1]=%p\n", frame);

	  frame[0] = gc_alloc(OBJ_BOXED_ARRAY,sizeof(long)*2);
	lbl1:;
	  gc_collect();
	lbl2:;
	  gc_collect();
	lbl3:;
	  gc_collect();
	  return;
	end:;
	  static int bitmap[] = {1,0};
	  static Frame frames[] = {
	    {&&lbl1,1,&bitmap[0]},
	    {&&lbl2,1,&bitmap[0]},
	    {&&lbl3,0,&bitmap[0]},
	    {&&end,0,NULL},
	  };
	  static FrameMap f = {1, (void*)test,&&end, frames, NULL};
	  gc_add_frame_map(&f); start_ptr=&&start; goto start;
	}

gc\_allocやgc\_collectの前後で状態を変える事が出来ます。

lbl1,lbl2はframe[0]が使用しますが、lbl3はframe[0]が使用しません。
endではビットマップ数が0なのでビットマップ領域を使わず、ビットマップを検索せず、領域も使いません。
lbl1とlbl2は同じ値をさしているので、lbl2に統合する事が出来るでしょう。
つまり以下のように書き換える事が出来ます。

	  frame[0] = gc_alloc(OBJ_BOXED_ARRAY,sizeof(long)*2);
	  gc_collect();
	lbl2:;
	  gc_collect();
	  gc_collect();
	  return;
	end:;
	  static int bitmap[] = {1};
	  static Frame frames[] = {
	    {&&lbl2,1,&bitmap[0]},
	    {&&end,0,NULL},
	  };

次にGCの書き換えを見てみましょう。GCのマークフェーズの関数gc_markは若干書き換えます。

	void gc_mark(void** ptr, void* addr) {
	  do {
	    addr = ptr[1];
	    ptr = (void**)(ptr[0]);

	    FrameMap* frame_map = gc_mark_find_frame_map(addr);
	    if (!frame_map) continue;

frame_mapを見つけた後さらに、Frameを見つけます。

	    Frame* frame = gc_mark_find_frame(frame_map, addr);
	    if (!frame) continue;

	    printf("find frame bitmap_size=%d\n", frame->bitmap_size);

フレームを使ってマークします。

	    Object** objects = (Object**)&ptr[-1 - frame_map->frame_size];
	    gc_mark_frame(frame, objects);

	  } while(ptr < gc_top_ptr);
	}

詳細なフレームの検索は、ラベルがアドレスより小さい物を見つけます。frame[1]は次のフレームです。

	Frame* gc_mark_find_frame(FrameMap* f, void* addr) {
	  Frame* frames = f->frames;
	  while (frames) {
	    if (frames[1].lbl > addr) return frames;
	    frames++;
	  }
	  return NULL;
	}

framesがNULLになる事は実はないはずです。
なぜならば、アドレスは常に最後のアドレスより手前にあるはずだからです。
しかしアドレス登録を間違えていた場合に供えてNULLチェックしています。
テーブルをNULL終端にすればより安心です。


bitmapを使ったマークは、ビットマップサイズ分ループしてビットマップのビットをチェックしてマークを呼び出します。

	void gc_mark_frame(Frame *frame, Object** objects) {
	  for(int i = 0; i < frame->bitmap_size; i++) {
	    unsigned int bitmap = frame->bitmap[i];
	    int n = i * 32;
	    while (bitmap) {
	      if(bitmap & 1) {
	        gc_mark_object(objects[n]);
	      }
	      bitmap = bitmap >> 1;
	      n++;
	    }
	  }  
	}

このようにすることで、より詳細なフレームマップを使ったGCを使う事が出来ます。

# 7. まとめ

ここでは、理解しやすくするためC言語を使って実装しながらスタックマップを使ったGCについて説明しました。
スタックマップを使うと、実行アドレスからのフレーム情報を取得する事が出来ます。
実行アドレスは、スタックトレースのリターンアドレスを見る事で取得できます。
フレームのアドレスもスタックをトレースする事で分かりました。
フレームのサイズは実行時に分からないので、フレームマップから取得する事にしました。
C言語ではラベルを関数の外側に持ち出す事が出来ないため、フレームマップの保存は最初の関数起動時にのみ行うことにしました。
gc時のマークフレーズはスタックをトレースし、アドレスからフレームマップを使ってフレームのサイズを求め、マークする事にしました。
6章では、ビットマップを使う事でさらに細かな情報を扱う事が出来るようにしました。

LLVMのスタックマップも同様の事が出来るようにする事が可能ですが、C++を使い、LLVMの詳細について詳しく知る必要がありました。
SML#ではさらにSMLの知識や、ランタイムを読む必要がありました。
ここでは、Cのみの知識で理解出来るようにしてスタックマップを説明しました。

# 8. 参考文献

C言語でバックトレース

http://www35.atwiki.jp/futoyama/pages/101.html

LLVMのガーベジコレクションのスタックマップ

http://llvm.org/docs/GarbageCollection.html#stack-map


- LLVMのGC(ガベージコレクション)サポートを使ってみる - Aizu Advent Calendar 2013(1日目)

	http://yutopp.hateblo.jp/entry/2013/12/01/000152

	https://github.com/yutopp/llvm-gc-support-test/tree/master/shadow_stack

- LLVM Garbage Collection.

	http://llvm.org/docs/GarbageCollection.html

- Appel89

	 Andrew W. Appel. Lisp and Symbolic Computation 19(7):703-705, July 1989.

- Goldberg91
	
	Tag-free garbage collection for strongly typed programming languages. Benjamin Goldberg. ACM SIGPLAN PLDI‘91.

- Tolmach94

	Tag-free garbage collection using explicit type parameters. Andrew Tolmach. Proceedings of the 1994 ACM conference on LISP and functional programming.

- Henderson2002

	[Accurate Garbage Collection in an Uncooperative Environment](http://citeseer.ist.psu.edu/henderson02accurate.html)
