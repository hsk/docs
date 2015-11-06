/*
 * heap_cheney.c
 * @copyright (c) 2007, Tohoku University.
 * @author UENO Katsuhiro
 * @version $Id: heap.c,v 1.10 2008/12/10 03:23:23 katsu Exp $
 */

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*#define DEBUG_USE_MMAP*/

#if defined(DEBUG) && defined(DEBUG_USE_MMAP)
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */
#if !defined(HAVE_CONFIG_H) || defined(HAVE_SYS_MMAN_H)
#include <sys/mman.h>
#endif /* HAVE_SYS_MMAN_H */
#endif /* DEBUG && DEBUG_USE_MMAP */

#include "smlsharp.h"
#include "object.h"
#include "objspace.h"
#include "heap.h"

/*#define GCSTAT*/
/*#define GCTIME*/

/*
 * Heap Space Layout:
 *
 *   INITIAL_OFFSET
 *    <-->
 *   +----+--------------------------------+
 *   |    |                                |
 *   +----+--------------------------------+
 *   ^    ^           ^                     ^
 *   base |          free                   limit
 *        |
 *        start address of free.
 *
 * Allocation:
 *
 * heap_space.free always points to the free space for the next object.
 * inc must be aligned in MAXALIGN, i.e., rounded by HEAP_ROUND_SIZE.
 *
 * h : size of object header
 * size : total object size intended to be allocated.
 * inc = HEAP_ROUND_SIZE(size)
 *
 *         |<---------------- inc ----------------->|
 *         |                                        |
 *         |     |<----------------- inc ---------------->|
 *         |     |                                  |     |
 *         |<------------ size ------------>|       |     |
 *         |     |                          |       |     |
 *         |<-h->|                          |       |<-h->|
 *         |     |                          |       |     |
 *         |  MAXALIGN                      |       |  MAXALIGN
 *   HEAP        v                                        v
 *    -----+-----+--------------------------+-------+-----+----------
 *         |head1|           obj1           |       |     |
 *    -----+-----+--------------------------+-------+-----+----------
 *               ^                                        ^
 *              prev                                     new
 *              free                                     free
 */
struct heap_space {
	char *free;
	char *limit;
	char *base;
};
extern struct heap_space sml_heap_from_space;

#define INITIAL_OFFSET  ALIGNSIZE(OBJ_HEADER_SIZE, MAXALIGN)

#define GC_FORWARDED_FLAG     OBJ_GC1_MASK
#define OBJ_FORWARDED         OBJ_GC1
#define OBJ_FORWARD_PTR(obj)  (*(void**)(obj))

#define IS_IN_HEAP_SPACE(heap_space, ptr) \
	((heap_space).base <= (char*)(ptr) \
	 && (char*)(ptr) < (heap_space).limit)

/* For each object, its size must be enough to hold one forwarded pointer. */
#define HEAP_ALLOC_SIZE_MIN  (OBJ_HEADER_SIZE + sizeof(void*))

struct heap_space sml_heap_from_space = {0, 0, 0};
static struct heap_space sml_heap_to_space = {0, 0, 0};

static void
heap_space_alloc(struct heap_space *heap, size_t size)
{
	size_t pagesize = getpagesize();
	size_t allocsize;
	void *page;

	allocsize = ALIGNSIZE(size, pagesize);
	page = xmalloc(allocsize);

	heap->base = page;
	heap->limit = page + allocsize;
}

static void
heap_space_swap()
{
	struct heap_space tmp_space;

	tmp_space = sml_heap_from_space;
	sml_heap_from_space = sml_heap_to_space;
	sml_heap_to_space = tmp_space;
}

static void
heap_space_free(struct heap_space *heap)
{
	free(heap->base);
}

static void
heap_space_clear(struct heap_space *heap)
{
	heap->free = heap->base + INITIAL_OFFSET;
	ASSERT(heap->free < heap->limit);
}

static void
heap_space_init(struct heap_space *heap, size_t size)
{
	if (size < INITIAL_OFFSET)
		size = INITIAL_OFFSET;
	heap_space_alloc(heap, size);
	heap_space_clear(heap);
}

#define HEAP_START(h)  ((h).base + INITIAL_OFFSET)
#define HEAP_USED(h)   ((size_t)((h).free - (h).base))
#define HEAP_REST(h)   ((size_t)((h).limit - (h).free))
#define HEAP_TOTAL(h)  ((size_t)((h).limit - HEAP_START(h)))


void
sml_heap_init(size_t size, size_t max_size ATTR_UNUSED)
{
	size_t space_size;

	space_size = size / 2;
	heap_space_init(&sml_heap_from_space, space_size);
	heap_space_init(&sml_heap_to_space, space_size);

}

void
sml_heap_free()
{
	heap_space_free(&sml_heap_from_space);
	heap_space_free(&sml_heap_to_space);
}

SML_PRIMITIVE void
sml_write(void *objaddr, void **writeaddr, void *new_value)
{
	*writeaddr = new_value;

	if (IS_IN_HEAP_SPACE(sml_heap_from_space, writeaddr))
		return;

	ASSERT(!IS_IN_HEAP_SPACE(sml_heap_to_space, writeaddr));
	ASSERT(!IS_IN_HEAP_SPACE(sml_heap_to_space, *writeaddr));

	/* remember the writeaddr as a root pointer which is outside
	 * of the heap. */
	sml_global_barrier(writeaddr, objaddr);
}

static void
forward(void **slot)
{
	void *obj = *slot;
	size_t obj_size, alloc_size;
	void *newobj;

	if (!IS_IN_HEAP_SPACE(sml_heap_from_space, obj)) {
		DBG(("%p at %p outside", obj, slot));
		ASSERT(!IS_IN_HEAP_SPACE(sml_heap_to_space, obj));
		if (obj != NULL)
			sml_trace_ptr(obj);
		return;
	}

	if (OBJ_FORWARDED(obj)) {
		*slot = OBJ_FORWARD_PTR(obj);
		GCSTAT_FORWARD_COUNT();
		DBG(("%p at %p forward -> %p", obj, slot, *slot));
		return;
	}

	obj_size = OBJ_TOTAL_SIZE(obj);
	alloc_size = HEAP_ROUND_SIZE(obj_size);

	ASSERT(HEAP_REST(sml_heap_to_space) >= alloc_size);

	newobj = sml_heap_to_space.free;
	sml_heap_to_space.free += alloc_size;
	memcpy(&OBJ_HEADER(newobj), &OBJ_HEADER(obj), obj_size);
	GCSTAT_COPY_COUNT(obj_size);

	DBG(("%p at %p copy -> %p (%lu/%lu)",
	     obj, slot, newobj,
	     (unsigned long)obj_size, (unsigned long)alloc_size));

	OBJ_HEADER(obj) |= GC_FORWARDED_FLAG;
	OBJ_FORWARD_PTR(obj) = newobj;
	*slot = newobj;
}

#define forward_children(obj)  sml_obj_enum_ptr(obj, forward)

static void
forward_region(void *start)
{
	char *cur = start;

	DBG(("%p - %p", start, sml_heap_to_space.free));

	while (cur < sml_heap_to_space.free) {
		forward_children(cur);
		cur += HEAP_ROUND_SIZE(OBJ_TOTAL_SIZE(cur));
	}
}

static void
forward_deep(void **slot)
{
	void *cur = sml_heap_to_space.free;
	forward(slot);
	forward_region(cur);
}

static void
do_gc(void)
{

	DBG(("start gc (%lu/%lu used) %p -> %p",
	     (unsigned long)HEAP_USED(sml_heap_from_space),
	     (unsigned long)HEAP_TOTAL(sml_heap_from_space),
	     sml_heap_from_space.base, sml_heap_to_space.base));

	sml_rootset_enum_ptr(forward);

	DBG(("copying root completed"));

	/* forward objects which are reachable from live objects. */
	forward_region(HEAP_START(sml_heap_to_space));

	sml_malloc_pop_and_mark(forward_deep);

	/* check finalization */
	sml_check_finalizer(forward_deep);

	/* clear from-space, and swap two spaces. */
	heap_space_clear(&sml_heap_from_space);
	heap_space_swap();

	/* sweep malloc heap */
	sml_malloc_sweep();

	DBG(("gc finished. remain %lu bytes",
	     (unsigned long)HEAP_USED(sml_heap_from_space)));

}

void
sml_heap_gc(void)
{
	do_gc();
	sml_run_finalizer(NULL);
}

static NOINLINE void *
slow_alloc(size_t obj_size)
{
	void *obj;

	GCSTAT_TRIGGER(obj_size);
	do_gc();

	if (HEAP_REST(sml_heap_from_space) >= obj_size) {
		obj = sml_heap_from_space.free;
		sml_heap_from_space.free += obj_size;
	} else {
		sml_fatal(0, "heap exceeded: intended to allocate %lu bytes.",
			  (unsigned long)obj_size);
	}

	obj = sml_run_finalizer(obj);
	return obj;
}

SML_PRIMITIVE void *
sml_alloc(unsigned int objsize, void *frame_pointer)
{
	/* objsize = payload_size + bitmap_size */
	void *obj;
	size_t inc = HEAP_ROUND_SIZE(OBJ_HEADER_SIZE + objsize);

	obj = sml_heap_from_space.free;
	if ((size_t)(sml_heap_from_space.limit - (char*)obj) >= inc) {
		sml_heap_from_space.free += inc;
	} else {
		sml_save_frame_pointer(frame_pointer);
		obj = slow_alloc(inc);
	}

	OBJ_HEADER(obj) = 0;
	return obj;
}
