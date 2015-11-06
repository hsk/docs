#include "smlsharp.h"
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <setjmp.h>
#include <stdio.h>
#include "control.h"
#include "frame.h"
#include "object.h"
void SMLmain() {
	sml_set_verbose(MSG_DEBUG);
	void *dummy_frame[3];
	printf("test %p %p %p\n", dummy_frame[0], dummy_frame[1], dummy_frame[2]);
	FRAME_HEADER(&dummy_frame[1]) = 0;
	printf("set ok %p %p %p\n", dummy_frame[0], dummy_frame[1], dummy_frame[2]);

	sml_control_start(&dummy_frame[1]);

	printf("test %p %p %p\n", dummy_frame[0], dummy_frame[1], dummy_frame[2]);

	void* data;
	sml_write(NULL, &data, sml_obj_alloc(OBJTYPE_BOXED_ARRAY, 5000));
	printf("frameptr %p\n", sml_load_frame_pointer());
	printf("GC start\n");
	sml_objspace_dump();
	sml_heap_gc();
	printf("GC end \n");
	sml_objspace_dump();

	sml_control_finish(&dummy_frame[1]);
}

