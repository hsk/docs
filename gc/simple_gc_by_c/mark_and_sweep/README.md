# Cだけで使える簡単な完全なGC

このサンプルプログラムはC言語だけで使えるとても簡単な完全なGCを行うサンプルです。
特に何も必要とせずに、完全なGCを実現出来ます。
メモリ効率はあまりよくありませんが、osx 上で32bit,64bitのCPUで動作させる事が出来ます。
WindowsやLinuxでは確認してませんが、特別な事はしていないので動作しなくても多少の修正で動作させる事が出来るでしょう。

マークアンドスイープアルゴリズムは、今使っているオブジェクトから使っているオブジェクトを全てマークし、その後、マークされていないオブジェクトを解放するスイープを行う２つのフェーズからなるメモり管理方法です。
使っているオブジェクトは、何らかの形でGCに通知する必要があります。

GCに通知する方法は様々だからか、多くのGCの簡単な実装例には含まれていません<a name="r3"></a>[\[3\]](#3) <a name="r4"></a>[\[4\]](#4)。
シャドースタックを用いた例はLLVMやSML#などでありますが、ハードルが高く感じます。
C言語だけでも使える簡単な例が欲しいと思い作成しました。

## 1. 使い方

### 初期化と終了処理

メイン関数では、`gc_init`で初期化し、`gc_free`で終了処理をします。
メインの処理は_mainの箇所で行います。

	int main() {
	  gc_init();
	  _main();
	  gc_free();
	  return 0;
	}

### 関数

関数の開始位置では、`enum`を使って、変数のアドレスを定義します。
以下の例では変数`A`を定義しています。

	void _main() {
	  enum {FRAME_START, FRAME_SIZE, A, FRAME_END};

`enum`の次の行には `ENTER_FRAME_ENUM();`を記述します。

	  ENTER_FRAME_ENUM();

関数の最後には`LEAVE_FRAME();`を記述します。
`LEAVE_FRAME();`は関数から抜ける際は必ず必要です。

	  LEAVE_FRAME();
	}

関数内でペアを定義して、intの値を登録してみましょう。

	  // ペア
	  frame[A] = gc_alloc_pair();
	  frame[A]->pair.fst = gc_alloc_int(10);
	  frame[A]->pair.snd = gc_alloc_int(20);

以上のようにするとGCで管理されたペアを作り、intの値を設定する事が出来ます。

### GCの実行

GCをするには`gc_collect()`を呼びます。

	  gc_collect();

### BOXED配列

オブジェクトの配列は以下のように`gc_alloc_boxed_array`を使ってメモリを確保し、
field要素を使って情報にアクセスします。

	void test3() {
	  enum {FRAME_START, FRAME_SIZE, OBJ, unboxed, FRAME_END};
	  ENTER_FRAME_ENUM();

	  // オブジェクト配列
	  frame[OBJ] = gc_alloc_boxed_array(2);
	  frame[OBJ]->field[0] = gc_alloc_int(30);
	  frame[OBJ]->field[1] = gc_alloc_int(40);

	  printf("data3 = %p %d\n", frame[OBJ]->field[0], frame[OBJ]->field[0]->intv);
	  printf("data4 = %p %d\n", frame[OBJ]->field[1], frame[OBJ]->field[1]->intv);

	  gc_collect();
	  LEAVE_FRAME();
	}

### UNBOXED配列

UNBOXEDな配列は以下のように`gc_alloc_unboxed_array`を使ってメモリを確保し、chars,shots,
ints,longs,longlongs,uchars,ushorts,uints,ulongs,ulonglongs要素を使って情報にアクセスします。

	void test3() {
	  enum {FRAME_START, FRAME_SIZE, unboxed, FRAME_END};
	  ENTER_FRAME_ENUM();

	  // int配列
	  frame[unboxed] = gc_alloc_unboxed_array(sizeof(int)*2);
	  frame[unboxed]->ints[0] = 50;
	  frame[unboxed]->ints[1] = 60;

	  printf("data5 = %p %d\n", &frame[unboxed]->ints[0], frame[unboxed]->ints[0]);
	  printf("data6 = %p %d\n", &frame[unboxed]->ints[1], frame[unboxed]->ints[1]);

	  gc_collect();
	  LEAVE_FRAME();
	}

### レコード

レコードを使う場合は、下位のビットにレコードがポインタを持つか、持たないかを記述します。

	void test_record() {
	  enum {FRAME_START, FRAME_SIZE, A, FRAME_END};
	  ENTER_FRAME_ENUM();

	  // レコード
	  enum {RECORD_SIZE=3,RECORD_BITMAP=BIT(1)|BIT(2)};
	  frame[A] = gc_alloc_record(RECORD_SIZE);
	  frame[A]->longs[0] = 10; // undata
	  frame[A]->field[1] = gc_alloc_int(20);
	  frame[A]->field[2] = gc_alloc_int(30);
	  frame[A]->longs[RECORD_SIZE] = RECORD_BITMAP;// レコードのビットマップ(cpuビット数分でアラインする。ビットマップもcpu bit数)

	  gc_collect();
	  LEAVE_FRAME();
	}

### NULL ポインタ

GCの実行時には、ヒープから確保したデータのみをチェックします。
そのため、ヒープにアドレスが登録されていなければ、問題なく動作します。

## 2. 内部構成

このプログラムはヒープとフレームリスト、スタック、オブジェクト、GCから構成されています。

- プログラム
	- ヒープ
	- スタック
	- フレームリスト
	- GC
	- オブジェクト

	グローバル上にヒープやフレームリストが存在し、GCがメモリを管理してオブジェクトを生成したり、削除したりします。

		Global
		       frame_list -----+
		       heap_list ---+  |
		                    |  |
		       GC           |  |
		   +---- HEAP <-----+  |
		   |      | |          |
		   |      | +-----+    |
		   |      |       |    |
		   v      v       v    |
		live     live   death  |
		Object-->Object Object |
		   A                   |
		   |                   |
		   |   Native Function |
		   |     STACK         |
		   +------ FRAME <-----+
		           (root set)

- ヒープリスト

	mallocで確保したデータをリストにして並べたものをヒープと読んでいます。
	GCがアロケーションしたデータはオブジェクトと呼び`heap_list`に登録されます。

- スタックとフレームリスト

	関数内でスタック上に確保したObject*の配列をグローバル変数の`frame_list`に追加し、終わったら外します。
	`ENTER_FRAME_ENUM()`マクロがローカル変数の`frame`を`frame_list`への登録を行い、
	`LEAVE_FRAME()`マクロがローカル変数の`frame`を`frame_list`から外します。

- GC

	GCはマークフェーズとスイープフェーズに別れます。

	マークフェーズ時には`frame_list`(ルート集合)をトラバースしマークします。
	マークされたオブジェクトは生きています。

	スイープフェーズ時には`heap_list`を調べマークされていれば死んでいるため開放します。
	マークされていなければマークを消します。

- オブジェクト
	
	GCの仕組み自体だけでは、不便です。オブジェクトのメモリアロケーションとサポートをするために、
	便利な関数やマクロがあります。オブジェクトにはintや文字列用のbox化されていない配列、オブジェクトを格納するBOX化された配列、リスト専用のペア、構造体を表すレコードがあります。レコードはポインタの位置情報を持つビットマップを持ちます。

## 3. ソースコードリーディング

では、最後にソースを見ましょう。



	/*
	C だけで使える簡単な完全なGCをするためのサンプルプログラム
	*/

	#include <stdio.h>
	#include <stdlib.h>
	#include <setjmp.h>

インクルードが幾つかあります。

	//#define DEBUG

	#ifdef DEBUG
	#define debug printf
	#else
	#define debug noprintf
	void noprintf(char* str, ...){}
	#endif

ここはデバッグオプションです。

	typedef enum {
	  OBJ_BOXED_ARRAY,
	  OBJ_UNBOXED_ARRAY,
	  OBJ_PAIR,
	  OBJ_RECORD,
	} ObjectType;

オブジェクトのタイプ定義です。

	typedef struct ObjectHeader {
	  struct ObjectHeader* next;
	  unsigned int size;
	  unsigned char type;
	  unsigned char marked;
	} ObjectHeader;

オブジェクトのヘッダは次のObjectの手前に配置され、オブジェクトのサイズやマークビット等を持ちます。

	typedef union Object {
	  struct {
	    union Object *fst;
	    union Object *snd;
	  }pair;
	  union Object* field[0];

	  char charv;
	  short shortv;
	  int intv;
	  long longv;
	  long long longlongv;
	  char chars[0];
	  short shorts[0];
	  int ints[0];
	  long longs[0];
	  long long longlongs[0];

	  unsigned char ucharv;
	  unsigned short ushortv;
	  unsigned int uintv;
	  unsigned long ulongv;
	  unsigned long long ulonglongv;
	  unsigned char uchars[0];
	  unsigned short ushorts[0];
	  unsigned int uints[0];
	  unsigned long ulongs[0];
	  unsigned long long ulonglongs[0];
	} Object;

オブジェクトはユニオンで様々なデータのアクセスを楽にします。

	typedef struct Frame {
	  struct Frame* frame_prev;
	  unsigned long frame_size;
	  Object* frame_data[0];
	} Frame;

フレーム構造体は、フレームのリストを構成します。

	Frame* frame_list;
	ObjectHeader* heap_list;
	int heap_num;
	int heap_max;

フレームのリストと、ヒープのリストを構成するグローバル変数があります。

	int heap_find(ObjectHeader* o) {
	  ObjectHeader* object = heap_list;
	  while (object) {
	    if(object == o) return 1;
	    object = object->next;
	  }
	  return 0;
	}

ヒープ内にオブジェクトが存在するかどうかを調べます。

次の`gc_collect`のマークフェーズである`gc_mark`で使われ`gc_mark_object`はオブジェクトをマークします。

	void gc_mark_object(Object* object) {

Objectのポインタの手前にヘッダがあります。

	  ObjectHeader* head = &((ObjectHeader*)object)[-1];
	  debug("mark %p\n",head);
	  long size;

ヒープに含まれないか、マークされていれば抜けます。

	  if (!heap_find(head)) return;
	  if (head->marked) return;

マークをして

	  long* bitmap;
	  head->marked = 1;

オブジェクトのタイプで構造を持つ場合は内部もトラバースします。

	  switch(head->type) {

配列の場合はサイズ分ループします。

	    case OBJ_BOXED_ARRAY:
	      size = ((int)head->size) / sizeof(long);
	      debug("size=%ld\n",size);
	      for(int i = 0; i < size; i++)
	          gc_mark_object(object->field[i]);
	      break;

ペアならペアの２つのポインタをトラバースします。

	    case OBJ_PAIR:
	      debug("PAIR\n");
	      gc_mark_object(object->pair.fst);
	      gc_mark_object(object->pair.snd);
	      break;

BOX化されていない配列は何もしません。

	    case OBJ_UNBOXED_ARRAY:
	      break;

レコードは、内部にBOX化領域とそうではない領域を持ちます。
BOX化領域のビットマップをもちそのビットが立っている領域だけをマークします。

	    case OBJ_RECORD:
	      size = ((int)head->size) / sizeof(long);
	      debug("RECORD size=%ld\n", size);
	      bitmap = &object->longs[size];
	      debug("size=%ld\n",size);
	      for(int i = 0; i < size; i++) {
	        if(bitmap[i/sizeof(long)] & (1 << (i % sizeof(long))))
	          gc_mark_object(object->field[i]);
	        else
	          debug("skip %d\n", i);
	      }
	      break;
	  }
	}

`gc_mark`はGCのマークフェーズでフレームリストをマークします。

	void gc_mark() {
	  Frame* frame = frame_list;
	  while(frame) {
	    for(int i = 0; i < frame->frame_size; i++)
	      gc_mark_object(frame->frame_data[i]);
	    frame = frame->frame_prev;
	  }
	}

`gc_sweep`はGCのスイープフェーズの処理を行います。すなわち、GCで配置されたデータの塊であるheap_listをトラバースしてマークされていれば解放し、リストから外します。マークされていればマークビットを下ろします。

	void gc_sweep() {
	  ObjectHeader** object = &heap_list;
	  while (*object) {
	    if (!(*object)->marked) {
	      ObjectHeader* unreached = *object;
	      *object = unreached->next;
	      free(unreached);

	      heap_num--;
	    } else {
	      (*object)->marked = 0;
	      object = &(*object)->next;
	    }
	  }
	}

`gc_collect`はマークしてスイープします。残っている数もカウントしてデバッグ時には表示します。

	void gc_collect() {
	  int prev_num = heap_num;

	  gc_mark();
	  gc_sweep();

	  heap_max = prev_num * 2;

	  debug("Collected %d objects, %d remaining.\n", prev_num - heap_num,
	         heap_num);
	}

`gc_alloc`は低レベル命令で、オブジェクトの種類とサイズを渡してメモリ確保し、ヒープリストに加えます。
ヒープのサイズがmaxに達するとgcを呼び出します。

	void* gc_alloc(ObjectType type, int size) {
	  if (heap_num == heap_max) gc_collect();

	  ObjectHeader* head = malloc(sizeof(ObjectHeader)+size);

	  debug("gc_alloc %p\n", head);
	  head->type = type;
	  head->next = heap_list;
	  heap_list = head;
	  head->marked = 0;
	  head->size=size;
	  heap_num++;

	  return &head[1];
	}

ここからはgc_allocの高レベル命令です。ペアや、BOX配列、UNBOX配列、レコードがあります。

	#define gc_alloc_pair() (gc_alloc(OBJ_PAIR, sizeof(Object*)*2))
	#define gc_alloc_boxed_array(size) (gc_alloc(OBJ_BOXED_ARRAY, sizeof(Object*)*size))
	#define gc_alloc_unboxed_array(size) (gc_alloc(OBJ_UNBOXED_ARRAY, size))
	#define gc_alloc_record(n) (gc_alloc(OBJ_RECORD, sizeof(Object*)*n+RECORD_BITMAP_NUM(n)))

レコード用にはビットマップのサイズを求めたり、ビットの数を求めるマクロがあります。

	#define RECORD_BITMAP_NUM(n) (((n)+sizeof(long)*8-1) / (sizeof(long)*8) )
	#define BIT(n) (1 << n)

box化されたint値を作成する関数もありますが、内部実装は配列です。

	void* gc_alloc_int(int n) {
	  int* data = gc_alloc(OBJ_UNBOXED_ARRAY, sizeof(int)*1);

	  debug("int ptr %p\n", data);
	  *data = n;
	  return data;
	}

フレームの開始と終了をするマクロがあります。
ENTER_FRAMEでは、このあと、フレームの繋ぎ替えをおこない、サイズを登録します。
フレームはObject*のローカルな配列変数にすぎません。

	#define ENTER_FRAME(SIZE) \
	  Object* frame[SIZE+2]; \
	  ((Frame*)frame)->frame_prev = frame_list; \
	  ((Frame*)frame)->frame_size = SIZE; \
	  frame_list = (Frame*)frame; \

ENTER\_FRAME\_ENUMはenumを使って記述しやすい補助的なマクロです。

	#define ENTER_FRAME_ENUM() ENTER_FRAME((FRAME_END-2))

LEAVE\_FRAMEは関数を抜ける前に呼び出し、フレームのリストを元に戻します。

	#define LEAVE_FRAME() \
	  frame_list = frame_list->frame_prev;


`gc_init`は変数を初期化します。

	void gc_init() {
	  frame_list = NULL;
	  heap_list = NULL;
	  heap_num = 0;
	  heap_max = 8;
	}

`gc_free`はメモリを解放します。`frame_list`がNULLならGCすれば全部消えるという訳です。

	void gc_free() {
	  frame_list = NULL;
	  gc_collect();
	}

ここからは関数を使うテストです。まずは、低レベルに自力でフレームを作り、フレームリストに登録し、メモリアロケーションを行って、gcを呼び出し、最後にフレームリストを戻して終わります。

	void test() {
	  void* frame[2+1];
	  frame[0] = (void*)frame_list;
	  frame[1] = (void*)1;
	  frame_list = (Frame*)frame;
	  frame[2] = gc_alloc(OBJ_BOXED_ARRAY,sizeof(long)*2);
	  gc_collect();
	  frame_list = frame_list->frame_prev;
	}

次の例は、マクロを使いフレームを操作し、配列も高レベルな命令を使って配置します。ずっと簡単です。

	void test2() {
	  ENTER_FRAME(1);
	  frame[2] = gc_alloc(OBJ_BOXED_ARRAY,sizeof(long)*2);
	  gc_collect();
	  LEAVE_FRAME();
	}

次の例は、enumを使ってスタックのサイズはマクロに任せています。ペア、BOX配列、int配列を作って、値を操作しています。
Objectがunionなので扱いが楽な事が分かると思います。

	void test3() {
	  enum {FRAME_START, FRAME_SIZE, A, B, unboxed, FRAME_END};
	  ENTER_FRAME_ENUM();

	  // ペア
	  frame[A] = gc_alloc_pair();
	  frame[A]->pair.fst = gc_alloc_int(10);
	  frame[A]->pair.snd = gc_alloc_int(20);

	  // オブジェクト配列
	  frame[B] = gc_alloc_boxed_array(2);
	  frame[B]->field[0] = gc_alloc_int(30);
	  frame[B]->field[1] = gc_alloc_int(40);

	  // int配列
	  frame[unboxed] = gc_alloc_unboxed_array(sizeof(int)*2);
	  frame[unboxed]->ints[0] = 50;
	  frame[unboxed]->ints[1] = 60;

	  printf("data1 = %p %d\n", frame[A]->pair.fst, frame[A]->pair.fst->intv);
	  printf("data2 = %p %d\n", frame[A]->pair.snd, frame[A]->pair.snd->intv);

	  printf("data3 = %p %d\n", frame[B]->field[0], frame[B]->field[0]->intv);
	  printf("data4 = %p %d\n", frame[B]->field[1], frame[B]->field[1]->intv);

	  printf("data5 = %p %d\n", &frame[unboxed]->ints[0], frame[unboxed]->ints[0]);
	  printf("data6 = %p %d\n", &frame[unboxed]->ints[1], frame[unboxed]->ints[1]);
	  gc_collect();
	  LEAVE_FRAME();
	}

次はintを使ってみただけです。

	Object* test_int(int n) {
	  enum {FRAME_START, FRAME_SIZE, A, FRAME_END};
	  ENTER_FRAME_ENUM();
	  frame[A] = gc_alloc_int(n);
	  LEAVE_FRAME();
	  return frame[A];
	}

次の例はレコードを使っています。0番目の領域はポインタを格納せず、1番目と2番目だけ格納するように書いてあります。BIT(1)|BIT(2)と書けばよいので使いやすいかと思います。

	void test_record() {
	  enum {FRAME_START, FRAME_SIZE, A, FRAME_END};
	  ENTER_FRAME_ENUM();

	  // レコード
	  enum {RECORD_SIZE=3,RECORD_BITMAP=BIT(1)|BIT(2)};
	  frame[A] = gc_alloc_record(RECORD_SIZE);
	  frame[A]->longs[0] = 10; // undata
	  frame[A]->field[1] = gc_alloc_int(20);
	  frame[A]->field[2] = test_int(30);
	  frame[A]->longs[RECORD_SIZE] = RECORD_BITMAP;// レコードのビットマップ(cpuビット数分でアラインする。ビットマップもcpu bit数)

	  gc_collect();
	  LEAVE_FRAME();
	}

最後がメイン関数です。

	int main() {
	  gc_init();
	  test();
	  gc_free();
	  printf("---\n");
	  gc_init();
	  test2();
	  gc_free();

	  printf("---\n");
	  gc_init();
	  test3();
	  gc_free();

	  printf("---\n");
	  gc_init();
	  test_record();
	  gc_free();
	  printf("sizeof type %ld header %ld\n", sizeof(ObjectType), sizeof(ObjectHeader));
	  return 0;
	}

全てソースを見終わりました。C言語は前方参照が出来ないのでボトムアップ的にプログラムを書かないと宣言を書く必要がでてきます。
逆に考えると、下から読めば、トップダウン的に読む事が出来ます。良ければもう一度上に読み返してみてください。

## 4. リファレンスマニュアル

- データ構造

	- enum ObjectType 

		オブジェクトの種類を表します。

			OBJ_BOXED_ARRAY   BOX化された配列
			OBJ_UNBOXED_ARRAY BOX化されていない配列
			OBJ_PAIR          ペア
			OBJ_RECORD        レコード

		ObjectHeader内に格納されます。

	- struct ObjectHeader

		オブジェクトをヒープに配置するときに使うヘッダ情報です。
		オブジェクトのポインタの手前に付きます。

		                 heap memory 

			heap_list -> | next pointer  |--+
		                 | size    4byte |  |
		                 | type    1byte |  |
		                 | marked  1byte |  |
		                 | align         |  |
		                 | align         |  |
		                 | Object data   |  |
		                 |               |  |
		                 |               |  |
		                                    |
			          +--| next pointer  |<-+
		              |  | :             |
		              |  | :             |
		              |
		              +->| next pointer  |--+
		                 | :             |  |
		                 | :             |  |

		                       :         :

		                                    |
			     NULL <--| next pointer  |<-+
		                 | :             |
		                 | :             |

		heap_listに追加されています。GC時にマークされなかった物は削除されます。
		typeと、markは下位ビットに押し込めればよりメモり効率が良くなるでしょう。

	- union Object

		Objectは様々なデータのunionです。
		オブジェクトデータにアクセスするための各フィールドが定義されています。

	- struct Frame

		完全なGCを行うために必要なポインタの情報を保持します。

		               machine stack

			frame_list -> | prev   |---+
			              | size   |   |
			              | frame0 |   |
			              | frame1 |   |
			              | frame2 |   |
			              | frame3 |   |
			              | frame4 |   |
			              |   :    |   |
			           +--| prev   |<--+
			           |  | size   |
			           |  | frame0 |
			           |  |   :    |
			           +->| prev   |---+
			              | size   |   |
			              | frame0 |   |
			              |   :    |   |
			              | NULL   |<--+
			              | size   |   
			              | frame0 |   
			              |   :    |

		この図ではスタックは上に成長するように書かれています。

		Frame情報はflame_listに保持されます。
		リストのトップのポインタはグローバル変数に保持されますが、フレームのデータはスタック上に取られるため、ネイティブのスタック上に取られるので高速に動作します。

- グローバル変数

	- Frame* frame_list

		フレーム情報を格納します。frame_prevが前のフレームのアドレスをさします。

	- ObjectHeader* heap_list

		メモリ確保したヒープです。

		単純なリスト構造をしています。GC時にこの中味を検索するため、高速化するにはこのデータ構造を変えると良いでしょう。

	- int heap_num

		ヒープ内のデータ数です。

	- int heap_max

		ヒープ内のデータの最大値です。初期値は8で、heap_maxに到達すると、gcが実行され、倍のサイズに拡張されます。

- 関数

	- ヒープ

		- int heap_find(ObjectHeader* o)

			ヒープを検索します。線形に探索するため速いとは言えません。速度はオブジェクトの数に比例します。

	- GC

		- void gc\_init()

			GC用のデータ領域を初期化します。

		- void gc\_free()

			GC用のデータ領域を開放します。

		- void gc\_collect()

			GCを実行します。gc\_allocからも呼び出されます。

			- void gc\_mark()

				GCから呼ばれ、ルート集合(フレーム内データ)をトラバースしてマークします。

				- void gc\_mark\_object(Object* o)

					GCのgc_markから呼ばれ、オブジェクトをマークします。

			- void gc\_sweep()

				GCからよばれ、マークされていないオブジェクトを開放し、マークされていたらマークビットを下ろします。

		- void* gc\_alloc(ObjectType type, int size)

			メモリを確保し、ヒープへ保存します。typeはオブジェクトのタイプを、sizeはバイト単位でのサイズを指定します。

		- void* gc\_alloc\_int(int n)

			intの値を持ったオブジェクトを作成します。UNBOXEDな配列を使っています。

- マクロ

	- アロケーション

		- gc\_alloc\_pair()

			ペアを作成します。

		- gc\_alloc\_boxed\_array(size)

			BOX化されたオブジェクト配列を作成します。sizeが1で、1つのオブジェクトを格納出来ます。

		- gc\_alloc\_unboxed\_array(size)

			BOX化されてない配列を作成します。サイズはバイト単位で指定します。

		- gc\_alloc\_record(n)

			レコードを作成します。nはレコード数を指定します。

		- RECORD\_BITMAP\_NUM(n)

			レコード数に対応したビットマップのサイズを求めます。

	- フレーム
		- ENTER\_FRAME(SIZE)

			関数の開始を表します。SIZEにはフレームの大きさを指定します。

		- ENTER\_FRAME\_ENUM()

			enumを使った場合に関数の先頭に記述します。

		- LEAVE_FRAME()

			関数の最後に記述します。

# 5. 参考文献

- <a name="1"></a><a href="r1">[1]</a> [ガベージコレクションのアルゴリズムと実装2010/3/18](http://www.amazon.co.jp/%E3%82%AC%E3%83%99%E3%83%BC%E3%82%B8%E3%82%B3%E3%83%AC%E3%82%AF%E3%82%B7%E3%83%A7%E3%83%B3%E3%81%AE%E3%82%A2%E3%83%AB%E3%82%B4%E3%83%AA%E3%82%BA%E3%83%A0%E3%81%A8%E5%AE%9F%E8%A3%85-%E4%B8%AD%E6%9D%91-%E6%88%90%E6%B4%8B/dp/4798025623)
	中村 成洋  (著), 相川 光  (著), 竹内 郁雄 (監修, 監修)


- <a name="2"></a><a href="r2">[2]</a> GCアルゴリズム詳細解説 GC/standard/Mark&Sweep

	http://seesaawiki.jp/w/author_nari/d/GC/standard/Mark%26Sweep

- <a name="3"></a><a href="r3">[3]</a> minimum gc

	https://github.com/authorNari/minigc

- <a name="4"></a><a href="r4">[4]</a> Mark and Sweep Garbage Collection Algorithm 2013 Robert Nystrom

	https://github.com/Sam-Serpoosh/mark_and_sweep_gc

- <a name="5"></a><a href="r5">[5]</a> Baby's First Garbage Collector. Bob Nystrom DECEMBER 08, 2013

	http://journal.stuffwithstuff.com/2013/12/08/babys-first-garbage-collector/

- <a name="6"></a><a href="r6">[6]</a> SML#

	http://www.pllab.riec.tohoku.ac.jp/smlsharp/ja/

- <a name="7"></a><a href="r7">[7]</a> SML# ランタイム

	https://github.com/smlsharp/smlsharp/tree/master/src/runtime

- <a name="8"></a><a href="r8">[8]</a> LLVMのGC(ガベージコレクション)サポートを使ってみる - Aizu Advent Calendar 2013(1日目)

	http://yutopp.hateblo.jp/entry/2013/12/01/000152

	https://github.com/yutopp/llvm-gc-support-test/tree/master/shadow_stack

- <a name="9"></a><a href="r9">[9]</a> LLVM Garbage Collection.

	http://llvm.org/docs/GarbageCollection.html

- <a name="10"></a><a href="r10">[10]</a> Appel89

	 Andrew W. Appel. Lisp and Symbolic Computation 19(7):703-705, July 1989.

- <a name="11"></a><a href="r11">[11]</a> Goldberg91
	
	Tag-free garbage collection for strongly typed programming languages. Benjamin Goldberg. ACM SIGPLAN PLDI‘91.

- <a name="12"></a><a href="r12">[12]</a> Tolmach94

	Tag-free garbage collection using explicit type parameters. Andrew Tolmach. Proceedings of the 1994 ACM conference on LISP and functional programming.

- <a name="13"></a><a href="r13">[13]</a> Henderson2002

	[Accurate Garbage Collection in an Uncooperative Environment](http://citeseer.ist.psu.edu/henderson02accurate.html)
