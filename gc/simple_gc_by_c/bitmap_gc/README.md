# Cだけで使える簡単なBitmapGC


## 1. 単純なBitmap GC

mark\_and\_sweep を改良して、ビットマップを作ってみたいと思います。
ビットマップGCを使う前にヒープの高速化をしろよとか思うかもしれませんが、
ビットマップGCを理解するのが目的なので良いのです。

今までは、ヒープはリストにして持っていました。
まず、デカい配列を作って使います。

ヒープは、単純に伸びるようにして、リンクで繋ぎます。32個単位でビットマップが1つあり、それが永遠に続きます。超簡単です。4096バイト以上なら困るので今までのヒープを使います。

	heaptop [bitmap32     ]
	        [4096byte    0]<-- open bit
	        [4096byte    1]
	        [4096byte    2]
	        [4096byte    3]
	               :
	        [4096byte   32]
	        [bitmap32     ]
	        [4096byte    0]
	        [4096byte    1]
	               :
	        [4096byte   32]


4096バイト以下のサイズなら、単純にビットマップから取得します。
4096x32+32が1ブロックのサイズです。
番号を持ちます。

マーク時にはビットマップは消してから、マークします。
ヒープのエリアは決まっているので安心です。

ヒープのポインタは、今空いているビットをさしていて、ドンドン進みます。

gc.c

## 2. 成長出来るようにする

今度は成長出来るようにします。
メモリ領域が足りなくなったら伸ばせるようにしましょう。

	#ifdef MINGW32
	#define GetPageSize()  (64 * 1024)
	#define ReservePageError  NULL
	#define ReservePage(addr, size)	\
		VirtualAlloc(addr, size, MEM_RESERVE, PAGE_NOACCESS)
	#define ReleasePage(addr, size) \
		VirtualFree(addr, size, MEM_RELEASE)
	#define CommitPage(addr, size) \
		VirtualAlloc(addr, size, MEM_COMMIT, PAGE_EXECUTE_READWRITE)
	#define UncommitPage(addr, size) \
		VirtualFree(addr, size, MEM_DECOMMIT)
	#else
	#include <sys/mman.h>
	#define GetPageSize()  getpagesize()
	#define ReservePageError  ((void*)-1)
	#define ReservePage(addr, size) \
		mmap(addr, size, PROT_NONE, MAP_ANON | MAP_PRIVATE, -1, 0)
	#define ReleasePage(addr, size) \
		munmap(addr, size)
	#define CommitPage(addr, size) \
		mprotect(addr, size, PROT_READ | PROT_WRITE)
	#define UncommitPage(addr, size) \
		mmap(addr, size, PROT_NONE, MAP_ANON | MAP_PRIVATE | MAP_FIXED, -1, 0)
	#endif /* MINGW32 */

gc2.c 

## 3. 複数サイズに分けるBitmapのヒープを成長出来るようにする

次に階層構造を持ったBitmap GCを作ってみようと思います。
1章で作成したビットマップGCはサイズが全て4096と無駄の多い物でした。
ここでは、SMLSharpにならって、8,16,32,64,128,256,512,1024,2048,4096の10つのビットマップ領域を用意して
サイズごとにそれぞれ分けてビットマップを持つ事にします。
それだけで、メモリ効率は良くなるはずです。
また、オブジェクトヘッダサイズを含めて持つ事としましょう。

## 4. ビットマップの検索を高速化する。

ヒープの構造が遅かったので、高速化しましょう。
ビット数が少ないうちは、ヒープの検索はリニア検索でよいのですが、ヒープが大きくなると非効率になります。
そこで、32bitを1ビットで表すようにして、上位の人は下位のビットが全て立っていたら、1にすることにします。
こうすることで32x32=1024のデータを最悪でも32+32回の検索で見つける事が出来ます。

	
	1個あたり                                                         int数
	32648[0                                            1    ..  31] 1 個
    1024 [0        1        2     3 ...        31][0..31].. [0..31] 32 個
	32   [0 1..31][0 1..31][0..31] ...     [0..31]                  1024 個

1024回が64回になるのでかなり速いはずです。
3階層にすると、3万2000回が32*3=96で約100回です。
３階層を1ブロックとして、8x32648=128kが程のデータを最小のブロックサイズとしましょう。8バイトにはヘッダのサイズも加えます。
(1+32+1024)*4バイトをヘッダ領域にもち、その後データ領域が128k程で続くことにします。
