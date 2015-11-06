#include "smlsharp.h"
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <setjmp.h>
#include <stdio.h>
#include "control.h"
#include "frame.h"
#include "sml_obj.h"

void test1(void* dt1) {


	void** slot = sml_push_tmp_rootset(1);

	void** data;
	sml_write(slot, &data, sml_obj_alloc(OBJTYPE_BOXED_ARRAY, 5511));
//sml_obj_alloc(OBJTYPE_BOXED_ARRAY, 400);
//	data[0] = sml_obj_alloc(OBJTYPE_BOXED_ARRAY, 4);
//	data[1] = sml_obj_alloc(OBJTYPE_BOXED_ARRAY, 8);
	printf("GC start-----------\n");
	sml_objspace_dump();
	sml_heap_gc();
	printf("GC end----------- \n");
	sml_objspace_dump();
	//sml_heap_dump();
	sml_pop_tmp_rootset(slot);
}

void test_FRAME() {
	void* aaa = sml_load_frame_pointer();
	printf("test_FRAME %p \n", aaa);
	void *dummy_frame[3];
	FRAME_HEADER(&dummy_frame[1]) = 0;
	sml_control_start(&dummy_frame[1]);
	void** noremove_slot = sml_push_tmp_rootset(1);
	noremove_slot[0] = sml_obj_alloc(OBJTYPE_BOXED_ARRAY, 8666);

	void** slot = sml_push_tmp_rootset(1);

	slot[0] = sml_obj_alloc(OBJTYPE_BOXED_ARRAY, 8000);
	sml_pop_tmp_rootset(slot);

	sml_control_finish(&dummy_frame[1]);

}

void test_NOFRAME() {

	void** noremove_slot = sml_push_tmp_rootset(1);
	noremove_slot[0] = sml_obj_alloc(OBJTYPE_BOXED_ARRAY, 7666);

	void** slot = sml_push_tmp_rootset(1);

	slot[0] = sml_obj_alloc(OBJTYPE_BOXED_ARRAY, 7000);
	sml_pop_tmp_rootset(slot);
}

void test(void* dt1) {


	test_FRAME();
	sml_heap_gc();
	sml_objspace_dump();
	printf("GC end test_FRAME end******* \n");

	test_NOFRAME();
	sml_heap_gc();
	sml_objspace_dump();
	printf("GC end test_NOFRAME end******* \n");


	void** noremove_slot = sml_push_tmp_rootset(1);
	noremove_slot[0] = sml_obj_alloc(OBJTYPE_BOXED_ARRAY, 6666);

	void** slot = sml_push_tmp_rootset(3);


	slot[0] = sml_obj_alloc(OBJTYPE_BOXED_ARRAY, 5000);
	slot[1] = sml_obj_alloc(OBJTYPE_BOXED_ARRAY, 5555);
	slot[2] = sml_obj_alloc(OBJTYPE_BOXED_ARRAY, 5511);
	printf("GC start-----------\n");
	sml_objspace_dump();
	sml_heap_gc();
	printf("GC end----------- \n");
	sml_objspace_dump();
	sml_pop_tmp_rootset(slot);
	printf("pop end----------- \n");
	sml_objspace_dump();
	sml_heap_gc();
	printf("GC end----------- \n");
	sml_objspace_dump();
}

void SMLmain() {
	sml_set_verbose(MSG_DEBUG);
	void *dummy_frame[3];
	FRAME_HEADER(&dummy_frame[1]) = 0;
	sml_control_start(&dummy_frame[1]);
	test(NULL);
	sml_control_finish(&dummy_frame[1]);
	printf("protect end----------- \n");
	sml_objspace_dump();
	sml_heap_gc();
	printf("GC end----------- \n");
	sml_objspace_dump();
}

