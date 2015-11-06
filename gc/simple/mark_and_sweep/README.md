# Cだけで使える簡単な完全なGC

このサンプルプログラムはC言語だけで使えるとても簡単な完全なGCを行うサンプルです。
特に何も必要とせずに、完全なGCを実現出来ます。
メモリ効率はあまりよくありませんが、osx 上で32bit,64bitのCPUで動作させる事が出来ます。
Windowsでは確認してません。

## 使い方

### 初期化と終了処理

メイン関数では、`gc_init`で初期化し、`gc_free`で終了処理をします。
メインの処理は_mainで行います。

	int main() {
	  vm_init();
	  _main();
	  vm_free();
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

### NULL

GCをするさいは、ヒープから確保したデータのみをチェックします。
そのため、ヒープにアドレスが登録されていなければ、問題なく動作します。

## データ構造

このプログラムはヒープとフレームリスト、スタック、オブジェクト、GCから構成されています。

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

ネイティブの関数のスタックフレーム上に管理されたフレームのリストを構築します。
Objectはルート集合(フレーム)から参照されているか、ルート集合から参照されているオブジェクトから参照されていれば生きていますが、ヒープからのみ参照されている場合は死んでいるため、GC時に開放されます。
管理されたオブジェクトをスタック上に取る事は残念ながら出来ません。

### enum ObjectType 

オブジェクトの種類を表します。

	OBJ_BOXED_ARRAY   BOX化された配列
	OBJ_UNBOXED_ARRAY BOX化されていない配列
	OBJ_PAIR          ペア
	OBJ_RECORD        レコード

ObjectHeader内に格納されます。

### struct ObjectHeader

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

### union Object

Objectは様々なデータのunionです。
オブジェクトデータにアクセスするための各フィールドが定義されています。

### struct Frame

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

## グローバル変数

### Frame* frame_list

フレーム情報を格納します。frame_prevが前のフレームのアドレスをさします。

### ObjectHeader* heap_list

メモリ確保したヒープです。

単純なリスト構造をしています。GC時にこの中味を検索するため、高速化するにはこのデータ構造を変えると良いでしょう。

### int heap_num

ヒープ内のデータ数です。

### int heap_max

ヒープ内のデータの最大値です。初期値は8で、heap_maxに到達すると、gcが実行され、倍のサイズに拡張されます。

## 関数

### int heap_find(ObjectHeader* o)

ヒープを検索します。線形に探索するため速いとは言えません。O(N)です。

### void gc\_mark\_object(Object* o)

GCのgc_markから呼ばれ、オブジェクトをマークします。

### void gc\_mark()

GCから呼ばれ、ルート集合(フレーム内データ)をトラバースしてマークします。

### void gc\_sweep()

GCからよばれ、マークされていないオブジェクトを開放し、マークされていたらマークビットを下ろします。

### void gc\_collect()

GCを実行します。gc\_allocからも呼び出されます。

### void* gc\_alloc(ObjectType type, int size)

メモリを確保し、ヒープへ保存します。typeはオブジェクトのタイプを、sizeはバイト単位でのサイズを指定します。

### void* gc\_alloc\_int(int n)

intの値を持ったオブジェクトを作成します。UNBOXEDな配列を使っています。

### void gc\_init()

VMを初期化します。

### void gc\_free()

VMを終了します。

## マクロ

### RECORD\_BITMAP\_NUM(n)

レコード数に対応したビットマップのサイズを求めます。

### gc\_alloc\_pair()

ペアを作成します。

### gc\_alloc\_boxed\_array(size)

BOX化されたオブジェクト配列を作成します。sizeが1で、1つのオブジェクトを格納出来ます。

### gc\_alloc\_unboxed\_array(size)

BOX化されてない配列を作成します。サイズはバイト単位で指定します。

### gc\_alloc\_record(n)

レコードを作成します。nはレコード数を指定します。

### ENTER\_FRAME(SIZE)

関数の開始を表します。SIZEにはフレームの大きさを指定します。

### ENTER\_FRAME\_ENUM()

enumを使った場合に関数の先頭に記述します。

### LEAVE_FRAME()

関数の最後に記述します。
