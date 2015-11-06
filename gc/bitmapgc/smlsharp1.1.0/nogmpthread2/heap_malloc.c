/*
 * heap_malloc.c - use malloc heap as a main heap. (for test use)
 * @copyright (c) 2010, Tohoku University.
 * @author UENO Katsuhiro
 */

#include "smlsharp.h"
#include "objspace.h"
#include "heap.h"

void
sml_heap_init(size_t size ATTR_UNUSED, size_t max_size ATTR_UNUSED)
{
}

void
sml_heap_free()
{
}

static void
trace(void **slot)
{
	sml_trace_ptr(*slot);
}

void
sml_heap_gc()
{
	sml_rootset_enum_ptr(trace, MAJOR);
	sml_malloc_pop_and_mark(trace, MAJOR);
	sml_check_finalizer(trace, MAJOR);
	sml_malloc_sweep(MAJOR);
	sml_run_finalizer(NULL);
}

SML_PRIMITIVE void *
sml_alloc(unsigned int objsize, void *frame_pointer)
{
	sml_save_frame_pointer(frame_pointer);
	return sml_obj_malloc(objsize);
}

SML_PRIMITIVE void
sml_write(void *objaddr, void **writeaddr, void *new_value)
{
	*writeaddr = new_value;
	sml_global_barrier(writeaddr, objaddr);
}
