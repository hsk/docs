#include <stdint.h>
#include "smlsharp.h"
#include "object.h"
#include "timer.h"

void _SMLmain();
void test();


void* _SMLstackmap = {0};

void* data[10];

void test() {
	static void* dt = {data};


	printf("test %p start %p\n", test, CALLER_FRAME_END_ADDRESS());
	sml_control_start();
	printf("test push\n");
	sml_push_fp();
	printf("slot\n");
	void*** slot = sml_tmp_root();
	*slot = sml_obj_alloc(OBJTYPE_BOXED_ARRAY, 15000);
	printf("test pop\n");
	sml_pop_fp();
	printf("test finish\n");
	sml_control_finish();
	printf("test finish ok\n");
}

extern int main();

void _SMLmain() {
	sml_control_start();
//	_SMLstackmap[0]=_SMLmain;
//	_SMLstackmap[1]=CALLER_FRAME_END_ADDRESS();


	sml_set_verbose(MSG_DEBUG);
	int i;
	sml_timer_t b1,b2;
	sml_time_t t;
	volatile int n;

	//dump_layout();
	sml_push_fp();
	sml_timer_now(b1);

	void*** slot = sml_tmp_root();
	*slot = sml_obj_alloc(OBJTYPE_BOXED_ARRAY, 15000);

	slot[0][0]= sml_obj_alloc(OBJTYPE_UNBOXED_ARRAY, 55000);
//	void* array = sml_obj_alloc(OBJTYPE_UNBOXED_ARRAY, 55);
//	sml_write(slot, &slot[0][1], sml_obj_alloc(OBJTYPE_UNBOXED_ARRAY, 16));
	//slot[0][1] = sml_obj_alloc(OBJTYPE_BOXED_ARRAY, 16);
//	sml_write(slot, &slot[0][1], NULL);
//	*slot = 
	//sml_heap_dump();
	sml_objspace_dump();
	sml_heap_gc();
	sml_pop_fp();
printf("----\n");

	sml_push_fp();
	sml_objspace_dump();
	sml_heap_gc();


	sml_control_suspend();
	test();
	sml_control_resume();

	sml_pop_fp();




	sml_timer_now(b2);
	sml_timer_dif(b1,b2,t);
	printf(TIMEFMT" sec\n", TIMEARG(t));
	printf("\n");
	sml_control_finish();
}
