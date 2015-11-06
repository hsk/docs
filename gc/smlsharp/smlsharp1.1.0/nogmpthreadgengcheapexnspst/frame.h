/*
 * frame.h - SML# stack frame format
 * @copyright (c) 2009-2010, Tohoku University.
 * @author UENO Katsuhiro
 */
#ifndef SMLSHARP__FRAME_H__
#define SMLSHARP__FRAME_H__

/*
 * FrameLayout.sml も参照してください。
 *
 * フレームポインタは前のフレームポインタのメモリを保持したアドレスをさしてます。
 * The next (in the direction of stack growing) word of the previous frame pointer holds the frame header.
 * フレームポインタの前の次の(スタックの育つ方向の)ワードはフレームのヘッダを保持しています。
 * If the header indicates that there is an extra word, then the extra word appears at the next of the header.
 * もしもヘッダがextraワードをさしてたら、ヘッダの次にエクストラワードが現れます。
 * The size of both the header and the extra word is same as the size of pointers on the target platform.
 * ヘッダとエクストラワードの両方のサイズはターゲットプラットフォームのポインタのサイズと同じです。
 * For example, on a 32-bit architecture whose the stack grows down
 * (x86 etc.),
 * [fp + 0] is previous frame pointer, and
 * [fp - 4] is the relative address of frame info word.
 * [fp - 8] is for the extra word of frame header.
 * 例えば、32bitアーキティクチャ上ではしたにスタックが育ち、(x86 etc.)
 * [fp + 0] は前回のフレームポインタで、
 * [fp - 4] はフレームインフォのワードのアドレスに関連し、
 * [fp - 8] はフレームヘッダのエクストラワードです。
 *
 * Frame Stack Chain:
 *                                     :          :
 *                                     |          |
 *                                     +==========+ current frame begin
 *                                     |          |
 *            +--------+               :          :
 *            | header |-------------->|frame info|
 *            +--------+               :          :
 *     fp --->|  prev  |               |          |
 *            +--|-----+               +==========+ current frame end
 *               |                     |          |
 *               |                     :          :
 *               |                     |          |
 *               |                     +==========+ previous frame begin
 *               |                     |          |
 *               |   +--------+        :          :
 *               |   | header |------->|frame info|
 *               |   +--------+        :          :
 *               +-->|  prev  |        |          |
 *                   +---|----+        +==========+ previous frame end
 *                       |             |          |
 *                       :             :          :
 *
 * header:
 *  31                            2   1     0
 * +--------------------------------+----+----+
 * |           info-offset          |next| gc |
 * +--------------------------------+----+----+
 * MSB                                      LSB
 *
 * info-offset holds the high 30 bit of the offset of frame info of this frame from the frame pointer.
 * Low 2 bit is always 0.
 * info-offsetはフレームポインタのフレームのframe infoのオフセットの上位30bit 保持しています。
 * If info-offset is 0, this frame has no frame info and thus there is no boxed or generic slot in this frame.
 * info-offsetが0なら、このフレームはframe infoを持ちません。しなわちこのフレームにはboxedか一般スロットはありません。
 * If the pointer size is larger than 32 bit, info-offset field is expanded to the pointer size.
 * ポインタさいずが32bitを超える場合、info-offsetフィールドはポインタサイズに拡張されます。
 * 
 * If next bit is 1, the header has an extra word which holds the address of previous ML frame.
 * next bitが1のとき、ヘッダーはextra wordを持ち、これは、前のML frameのアドレスを保持します。
 * (this is used to skip C frames between two ML frames due to callback functions.)
 * (これは、Cフレームと２つのMLフレームの間のコールバック関数をスキップするために使われます)。
 *
 * gc bit is reserved for non-moving gc. It must be 0 for new frames.
 * gc ビットはnon-moving gcのために用意されています。 これは新しいフレームでは必ず0でなくてはなりません。
 * If the root-set enumerator meets this frame during pointer enumeration, the gc bit is set to 1.
 * もしもrootセットのenumeratorがこのトラバース時にフレームに遭遇したら、gcビットは1に設定されます。
 * 
 * To make sure that we may use last 2 bits for the flags, the frame info must be aligned at the address of multiple of 4.
 * フラグ用の最後の2ビットのために、アドレスは4bitでアラインされてるといいです。
 *
 * frame info:
 *  31                16 15                 0
 * +--------------------+--------------------+
 * |  num boxed slots   |  num bitmap bits   |
 * +--------------------+--------------------+
 *
 *  31                16 15                 0
 * +--------------------+--------------------+
 * |  boxed slots 数    |  bitmap bits 数     |
 * +--------------------+--------------------+
 *
 * The size of frame info is same as the size of pointers on the target platform.
 * フレームインフォのサイズはターゲットプラットフォームのサイズと同じです。
 * If the pointer size is larger than 32 bit, then padding bits must be added to the most significant side of the frame info.
 * このポインタサイズは32 bitよりおおきい場合、フレームインフォの美味くパディングさせてください。手抜き翻訳w
 *
 * Structure of Frame:
 * フレーム構造
 *
 * addr
 *   | :               :
 *   | +---------------+ [align in frameAlign] <------- offset origin
 *   | | pre-offset    |
 *   | +===============+ ================== frameの開始位置
 *   | |               |
 *   | +---------------+ [align in frameAlign]
 *   | | slots of tN   | tNの 一般 slot
 *   | |  :            |   :
 *   | +---------------+ [align in frameAlign]
 *   | :               :
 *   | +---------------+ [align in frameAlign]
 *   | | slots of t1   | t1の一般 slot 0
 *   | :               :   :
 *   | +---------------+ [align in frameAlign]
 *   | | slots of t0   | t0の一般 slot 0
 *   | |               | t0の一般 slot 1
 *   | :               :   :
 *   | +---------------+ [align in frameAlign] <---- ヘッダがさしてるところ
 *   | | frame info    |
 *   | +---------------+ [align in unsigned int]
 *   | |               | 
 *   | +---------------+ [align in void*]
 *   | | boxed part    | box化データの部分
 *   | :               :
 *   | |               |
 *   | +---------------+ [align in void*]
 *   | |               |
 *   | +---------------+ [align in unsigned int]
 *   | | sizes         | t0のスロット数
 *   | |               | t1のスロット数
 *   | :               :   :
 *   | |               | t(N-1)のスロット数
 *   | +---------------+ [align in unsigned int]
 *   | | bitmaps       | (t0-t31)のビットマップ
 *   | :               :   :
 *   | |               | (t(N-32)-t(N-1))のビットマップ
 *   | +---------------+ [align in unsigned int]
 *   | | unboxed part  | BOX化されていないデータ領域
 *   | |               |
 *   | |               |
 *   | :               :
 *   | |               |
 *   | +===============+ ================== end of frame
 *   | | post-offset   |
 *   | +---------------+ [align in frameAlign]
 *   | :               :
 *   v
 *
 *  要するに、フレームにデータを持ちたい場合は、frameをリンクするための配列と、フレーム用のデータ配列をスタックに取って美味く使うと良いです。
 *  で配列はアラインしておいて、使うのだけどサイズが必要で、使うと。
 * まず、アンボックス化されているパートの事は知った事ではないw
 * でスロット数分、データが持てる。
 * スロットは各自複数個のデータを持てて、そのかずが、sizesに書かれている。
 * 中味のデータはslots of tNに書かれていて、スロット数分のポインタが多分入っている。
 * ポインタはBOXパートを指し示しているか、unboxedのパートを多分さしている。という事を念頭に、control.cのソースを読むと良いと思う。
 * まず、boxedのサイズを求めて、フレームポインタからアラインした位置からBOXパートがならんでいる。
 * でそのアドレスを探すと良いみたい。
 * それで、次のsizesの位置が分かるので、そこに移動して、genericsのサイズ分
 * ぐるぐる回る。ジェネリックスのサイズ分したに行くとビットマップがある。
 * で、ジェネリックスのビットマップ分、ループして、ビットマップがアンボックスなら
 * 単純にスルーする。
 * そうでないときは、BOXかされているので、トレースが必要で、ジェネリック分もどってトレースする。
 * ということで、BOXデータだけ使う分にはサイズ指定して使うだけで良いようだ。
 * まず使ってみよう。
 */

#ifdef STACK_GROWSUP
#define FRAME_HEADER(fp)  (*(uintptr_t*)((void**)(fp) + 1))
#define FRAME_EXTRA(fp)   (*(uintptr_t*)((void**)(fp) + 2))
#else
#define FRAME_HEADER(fp)  (*(uintptr_t*)((void**)(fp) - 1))
#define FRAME_EXTRA(fp)   (*(uintptr_t*)((void**)(fp) - 2))
#endif
#define FRAME_NEXT(fp)  (((void**)(fp))[0])

#define FRAME_FLAG_VISITED  0x1
#define FRAME_FLAG_SKIP     0x2
#define FRAME_OFFSET_MASK   (~(uintptr_t)0x3)
#define FRAME_INFO_OFFSET(header)  ((intptr_t)((header) & FRAME_OFFSET_MASK))
#define FRAME_SKIP_NEXT(header)    ((void*)((header) & FRAME_OFFSET_MASK))

#define FRAME_NUM_BOXED(info)   (((unsigned int*)(info))[0] >> 16)
#define FRAME_NUM_GENERIC(info) (((unsigned int*)(info))[0] & 0xffff)
#define FRAME_BOXED_PART(info) \
	((void*)((char*)(info) + ALIGNSIZE(sizeof(unsigned int), \
					   sizeof(void*))))

#ifndef SIZEOF_GENERIC
#define SIZEOF_GENERIC MAXALIGN
#endif

#endif /* SMLSHARP__FRAME_H__ */
