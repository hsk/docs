#include "smlsharp.h"
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <setjmp.h>
#include <stdio.h>
#include "control.h"
#include "frame.h"
void SMLmain() {
	void *dummy_frame[3];
	void * fp;
	printf("test %p %p %p\n", dummy_frame[0], dummy_frame[1], dummy_frame[2]);
	FRAME_HEADER(&dummy_frame[1]) = 0;
	printf("set ok %p %p %p\n", dummy_frame[0], dummy_frame[1], dummy_frame[2]);

//	fp = sml_load_frame_pointer();
//	printf("fp %p\n", fp);

	sml_control_start(&dummy_frame[1]);

	fp = sml_load_frame_pointer();
	printf("fp %p\n", fp);

	printf("test %p %p %p\n", dummy_frame[0], dummy_frame[1], dummy_frame[2]);
	void* data;
	data = sml_alloc(1, dummy_frame[1]);
	printf("test %p\n", data);
	data = sml_alloc(1, fp);
	printf("test %p\n", data);

	sml_control_finish(&dummy_frame[1]);
}
