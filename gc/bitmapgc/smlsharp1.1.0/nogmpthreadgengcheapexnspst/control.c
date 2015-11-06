/*
 * frame.c
 * @copyright (c) 2007-2010, Tohoku University.
 * @author UENO Katsuhiro
 */

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <setjmp.h>
#include "smlsharp.h"
#include "sml_obj.h"
#include "frame.h"
#include "objspace.h"
#include "control.h"
/*
sml_control構造体
*/
struct sml_control {
	void *frame_stack_top; // フレームのトップ
	void *frame_stack_bottom;// フレームのボトム
	sml_obstack_t *tmp_root;      /* GCのテンポラリルートスロット */
};

// シングルスレッドだと全体で１つのコントロール構造体を持つ
static struct sml_control *global_control;

/*
コントロールの開始

フレームポインタを受け取ってフレームポインタは
コントロールのframe_stack_topとframe_stack_bottomに保存される。

最初は、global_controlがnullなので初期化され、2回目以降の呼び出しでは、
frame_stack_topが更新される。

*/
SML_PRIMITIVE void
sml_control_start(void *frame_pointer)
{
	if (global_control == NULL) {
		global_control = xmalloc(sizeof(struct sml_control));
		global_control->frame_stack_top = frame_pointer;
		global_control->frame_stack_bottom = frame_pointer;
		global_control->tmp_root = NULL;
		DBG(("START NEW THREAD : %p", pthread_self()));
	} else {
		FRAME_HEADER(frame_pointer) |= FRAME_FLAG_SKIP;
		FRAME_EXTRA(frame_pointer) =
			(uintptr_t)global_control->frame_stack_top;
		global_control->frame_stack_top = frame_pointer;
	}

}

/*
コントロールの終了

フレームポインタを受け取って終了処理をする。

フレームポインタがframe_stack_bottomと同じなら終了処理を行う。

フレームポインタがframe_stack_bottomでない場合は、リスト構造を作っているので、
一つリンクを外す。
*/
SML_PRIMITIVE void
sml_control_finish(void *frame_pointer)
{
	if (global_control->frame_stack_bottom == frame_pointer) {
		DBG(("FINISH THREAD : %p", pthread_self()));

		if (global_control == NULL)
			return;
		sml_obstack_free(&global_control->tmp_root, NULL);
		free(global_control);
		global_control = NULL;

	} else {
		ASSERT(FRAME_HEADER(frame_pointer) & FRAME_FLAG_SKIP);
		global_control->frame_stack_top = (void*)FRAME_EXTRA(frame_pointer);
	}
}

/*
一時的なルート集合を作って返す。
指定個数の配列を作って、tmp_rootに保存し配列のポインタを返す。
*/
void **
sml_push_tmp_rootset(size_t num_slots)
{
	void **ret;
	unsigned int i;

	ret = sml_obstack_alloc(&global_control->tmp_root, sizeof(void*) * num_slots);
	for (i = 0; i < num_slots; i++)
		ret[i] = NULL;
	return ret;
}

/*

一時的なルート集合を消す。

*/
void
sml_pop_tmp_rootset(void **slots)
{
	sml_obstack_free(&global_control->tmp_root, slots);
}

/*
スタックフレームのトップを更新する。
*/
SML_PRIMITIVE void
sml_save_frame_pointer(void *p)
{
	global_control->frame_stack_top = p;
}

/*
スタックフレームのトップを取得する。
*/
void *
sml_load_frame_pointer()
{
	return global_control->frame_stack_top;
}

/*

フレーム内の検索
１つ１つのフレームを見る。

*/
static void
frame_enum_ptr(void *frame_info, void (*trace)(void **))
{
	void **boxed;
	unsigned int *sizes, *bitmaps, num_generics, num_boxed;
	unsigned int i, j, num_slots;
	ptrdiff_t offset;
	char *generic;


	printf("frame_info %p\n", frame_info);

	// boxの数を取得する上位16bit
	num_boxed = FRAME_NUM_BOXED(frame_info);
	// 一般の数を取得 下位16bit

	num_generics = FRAME_NUM_GENERIC(frame_info);
	// BOXパートを取得 フレームポインタからアラインされた位置
	boxed = FRAME_BOXED_PART(frame_info);

	printf("num_boxed=%d num_generics=%d\n", num_boxed, num_generics);

	// BOX数だけboxedを見る。
	for (i = 0; i < num_boxed; i++) {
		if (*boxed)
			trace(boxed);
		boxed++;
	}

	offset = (char*)boxed - (char*)frame_info;
	offset = ALIGNSIZE(offset, sizeof(unsigned int));
	sizes = (unsigned int *)(frame_info + offset);
	bitmaps = sizes + num_generics;
	generic = frame_info;

	for (i = 0; i < num_generics; i++) {
		num_slots = sizes[i];
		if (BITMAP_BIT(bitmaps, i) == TAG_UNBOXED) {
			generic -= num_slots * SIZEOF_GENERIC;
			printf("gen unboxed %d\n", i);
		} else {
			printf("gen boxed %d\n", i);
			for (j = 0; j < num_slots; j++) {
				generic -= SIZEOF_GENERIC;
				printf("trace %p %p\n",(void**)generic, *(void**)generic);
				trace((void**)generic);
			}
		}
	}
}

/*
control->frame_stack_topから
control->frame_stack_bottom中のフレームを全て見る
*/
static void
stack_enum_ptr(void (*trace)(void **),
	       void *frame_stack_top, void *frame_stack_bottom)
{
	void *fp = frame_stack_top;
	uintptr_t header;
	intptr_t offset;

	for (;;) {
		// ヘッダを取得
		header = FRAME_HEADER(fp);
		// ヘッダにフラグを立てる
		FRAME_HEADER(fp) = header | FRAME_FLAG_VISITED;

		// フレームの情報のオフセットを取得
		offset = FRAME_INFO_OFFSET(header);
		// オフセットが0でなければ中味を走査する。フレーム内のboxedスロットを調べる
		if (offset != 0)
			frame_enum_ptr((char*)fp + offset, trace);

		/* When MINOR tracing, we need to trace not only unvisited
		 * frames but also the first frame of visited frames since
		 * the first frame may be modified by ML code from the
		 * previous frame tracing.
		 */

		// ボトムなら終わり
		if (fp == frame_stack_bottom)
			break;

		// フレームスキップなら、エクストラを設定する(C言語でsml_control_startを使って作った場合)
		if (header & FRAME_FLAG_SKIP)
			fp = (void*)FRAME_EXTRA(fp);
		// スキップでなければ、fpの次を設定する。
		else
			fp = FRAME_NEXT(fp);

		// つまり、sml_control_startを使わずに、直接フレームをsml_save_frame_pointerで変えて、
		// 元に戻していればextraは不要であるといえる。
	}

	DBG(("frame end"));
}

struct sml_obstack {
	struct sml_obstack *next;
	char *start, *end;
	char *free, *base;
};

/*

コントロール内のenum。
一時的なルート集合と
frame_stackをトラバースしてtraceを実行する。

*/
void
sml_control_enum_ptr(void (*trace)(void **))
{
	struct sml_control *control = global_control;

	if(control) {
		printf("frame_stack_top\n");

		stack_enum_ptr(trace, control->frame_stack_top,
			       control->frame_stack_bottom);

		printf("obstack\n");
		sml_obstack_t *obstack = control->tmp_root;
		while (obstack) {
			void *start = (void*)obstack->start;
			void *end = (void*)obstack->free;
			void **i;
			for (i = start; i < (void**)end; i++)
				trace(i);
			obstack = obstack->next;
		}
	}
}
