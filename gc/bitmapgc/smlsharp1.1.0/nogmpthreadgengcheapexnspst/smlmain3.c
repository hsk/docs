#include "smlsharp.h"
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <setjmp.h>
#include <stdio.h>
#include "control.h"
#include "frame.h"
#include "sml_obj.h"

/*
                       +-------------------+
                      0| slot t1 0         | 
                      1| align             | 
                      2|                   | 
                      3|                   | 
                       +-------------------+
                      4| slot t1 1         | 
                      5| align             | 
                      6|                   | 
                      7|                   | 
                       +-------------------+
                      8| slot t0 0         |
                      9| align             |
                      A|                   | 
frame->| prev |       B|                   |
       | head | ----> C|boxnum 0 |gennum 2 | <- frame_info
       | extra|       D| sizes             | size 0 <- 1 slot
                      E|                   | size 1 <- 2 slot
                       +-------------------+
                      F| bitmaps           | 1個
                       +-------------------+
*/
void test_FRAME_UNBOXED() {
	enum{
		LF_GEN_NUM=3,
		LF_BOXED_NUM=0,
		LF_INFO=LF_GEN_NUM * 4,
		LF_SIZE=LF_INFO+LF_BOXED_NUM,
		LF_BITMAP=LF_SIZE+LF_GEN_NUM,
	};
	volatile void* frame_info[1+LF_BOXED_NUM+LF_GEN_NUM+LF_GEN_NUM*4+1];// 0番目が今のところ、frame_info
	volatile void* frame[3];// ネスとして呼ばれるので、extraが必要
	frame_info[LF_INFO] = 2;// gennumの数を書く
	frame_info[LF_SIZE+1] = 1; // slot 0のサイズ
	frame_info[LF_SIZE+2] = 2; // slot 1のサイズ
	frame_info[LF_SIZE+3] = 3; // bitmap
	FRAME_HEADER(&frame[2]) = (uintptr_t)((int)&frame_info[LF_INFO] - (int)&frame[2]);// フレームのオフセット計算
	sml_control_start(&frame[2]);
	sml_heap_gc();
	frame_info[LF_INFO - 4 * 1] = sml_obj_alloc(OBJTYPE_UNBOXED_ARRAY, 100);// box化データに保存
	frame_info[LF_INFO - 4 * 2] = sml_obj_alloc(OBJTYPE_UNBOXED_ARRAY, 200);// box化データに保存
	frame_info[LF_INFO - 4 * 3] = sml_obj_alloc(OBJTYPE_UNBOXED_ARRAY, 201);// box化データに保存
	printf("----------------\n");
	printf("GC Start\n");
	sml_heap_gc();
	printf("GC end----------- \n");
	sml_objspace_dump();
	frame_info[LF_BITMAP] = 2; // bitmap
	sml_heap_gc();
	printf("GC end----------- \n");
	sml_objspace_dump();
ret:
	sml_control_finish(&frame[2]);
}

/*
frame->| prev |
       | head | ----> |boxnum 2 |gennum 0 | <- frame_info
       | extra|       |                   | boxed 0 <- 123
                      |                   | boxed 1 <- 456
*/
void test_FRAME_BOXED2() {
	enum{
		LF_GEN_NUM=0,
		LF_BOXED_NUM=2,
		LF_INFO=LF_GEN_NUM * 4,
		LF_SIZE=LF_INFO+LF_BOXED_NUM,
		LF_BITMAP=LF_SIZE+LF_GEN_NUM,
	};
	volatile void* frame_info[3];// 0番目が今のところ、frame_info
	volatile void* frame[3];// ネストして呼ばれるので、extraが必要
	frame_info[LF_INFO] = LF_BOXED_NUM << 16;// frameのサイズをframe_infoに書く
	FRAME_HEADER(&frame[2]) = (uintptr_t)((int)&frame_info[LF_INFO] - (int)&frame[2]);// フレームのオフセット計算
	sml_control_start(&frame[2]);
	frame_info[LF_INFO+1] = sml_obj_alloc(OBJTYPE_UNBOXED_ARRAY, 123);// box化データに保存
	frame_info[LF_INFO+2] = sml_obj_alloc(OBJTYPE_UNBOXED_ARRAY, 456);// box化データに保存
	printf("----------------\n");
	printf("GC Start\n");
	sml_heap_gc();
	printf("GC end----------- \n");
	sml_objspace_dump();
	sml_control_finish(&frame[2]);
}

void test_NO_CONTROL_UNBOXED() {
	enum{
		LF_GEN_NUM=3,
		LF_BOXED_NUM=0,
		LF_INFO=LF_GEN_NUM * 4,
		LF_SIZE=LF_INFO+LF_BOXED_NUM,
		LF_BITMAP=LF_SIZE+LF_GEN_NUM,
	};
	volatile void* frame_info[1+LF_BOXED_NUM+LF_GEN_NUM+LF_GEN_NUM*4+1];// 0番目が今のところ、frame_info
	volatile void* frame[2];// 自分で管理するのでextraはなし
	frame_info[LF_INFO] = 2;// gennumの数を書く
	frame_info[LF_SIZE+1] = 1; // slot 0のサイズ
	frame_info[LF_SIZE+2] = 2; // slot 1のサイズ
	frame_info[LF_SIZE+3] = 3; // bitmap
	frame[0] = (uintptr_t)((int)&frame_info[LF_INFO] - (int)&frame[1]);// フレームのオフセット計算
	frame[1] = sml_load_frame_pointer();// 前回を保存
	sml_save_frame_pointer(&frame[1]);// フレームを保存
	sml_heap_gc();
	frame_info[LF_INFO - 4 * 1] = sml_obj_alloc(OBJTYPE_UNBOXED_ARRAY, 100);// box化データに保存
	frame_info[LF_INFO - 4 * 2] = sml_obj_alloc(OBJTYPE_UNBOXED_ARRAY, 200);// box化データに保存
	frame_info[LF_INFO - 4 * 3] = sml_obj_alloc(OBJTYPE_UNBOXED_ARRAY, 201);// box化データに保存
	printf("----------------\n");
	printf("GC Start\n");
	sml_heap_gc();
	printf("GC end----------- \n");
	sml_objspace_dump();
	frame_info[LF_BITMAP] = 2; // bitmap
	sml_heap_gc();
	printf("GC end----------- \n");
	sml_objspace_dump();
ret:
	sml_save_frame_pointer(frame[1]);// 元に戻す
}

void test_NO_CONTROL_BOXED() {
	enum{ LF_BOXED_NUM=2 };
	volatile void* local_frame[1+LF_BOXED_NUM];// 0番目が今のところ、local_frame
	volatile void* frame[2];// extraなし
	local_frame[0] = LF_BOXED_NUM << 16;// frameのサイズをlocal_frameに書く
	frame[0] = (uintptr_t)((int)local_frame - (int)&frame[1]);// フレームのオフセット計算
	frame[1] = sml_load_frame_pointer();// 前回を保存
	sml_save_frame_pointer(&frame[1]);// フレームを保存
	local_frame[1] = sml_obj_alloc(OBJTYPE_UNBOXED_ARRAY, 123);// box化データに保存
	local_frame[2] = sml_obj_alloc(OBJTYPE_UNBOXED_ARRAY, 456);// box化データに保存
ret:
	sml_save_frame_pointer(frame[1]);// 元に戻す
}

#define BOXED_FRAME_START(frame,local) \
	volatile void** local[END_BOXED];\
	volatile void* frame[2];\
	local[0] = (END_BOXED-1) << 16;\
	frame[0] = (uintptr_t)((int)local - (int)&frame[1]);\
	frame[1] = sml_load_frame_pointer();\
	sml_save_frame_pointer(&frame[1]);

#define BOXED_FRAME_END(frame) \
	sml_save_frame_pointer(frame[1]);

enum{D,E};

void test_BOXED_MACRO() {
enum{START_BOXED,D,E,END_BOXED};
BOXED_FRAME_START(frame,local);
	int a = 100;
	int b = 2;
	int c = a + b;
	local[D] = sml_obj_alloc(OBJTYPE_BOXED_ARRAY, c);// box化データに保存
	local[E] = sml_obj_alloc(OBJTYPE_UNBOXED_ARRAY, 456);// box化データに保存
	local[D][0]=sml_obj_alloc(OBJTYPE_UNBOXED_ARRAY, 111);
	char* e = (char*)local[E]; // バイト配列として扱う。
	e[0] = 1;
	printf("GC start----------- \n");
	sml_heap_gc();
	printf("GC end----------- \n");
	sml_objspace_dump();

BOXED_FRAME_END(frame);
}
// ネイティブなSML#のコンパイラはここは、sml_control_start,sml_control_finishを呼び出してたのでまぁそういう事だ。
// SML#2.0.0だと、LLVMのフレームスタックを使うのでそうなる。

void SMLmain() {
	sml_set_verbose(MSG_DEBUG);
	volatile void *frame[3];
	FRAME_HEADER(&frame[2]) = 0;
	sml_control_start(&frame[2]);
	//test_FRAME_UNBOXED();
	//test_NO_CONTROL_UNBOXED();
	test_NO_CONTROL_BOXED();
	test_BOXED_MACRO();
	sml_control_finish(&frame[2]);
	printf("protect end----------- \n");
	sml_objspace_dump();
	sml_heap_gc();
	printf("GC end----------- \n");
	sml_objspace_dump();
}

