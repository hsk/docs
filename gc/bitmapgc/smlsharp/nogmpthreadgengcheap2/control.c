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
#include "object.h"
#include "frame.h"
#include "objspace.h"
#include "control.h"

struct sml_control {
	void *frame_stack_top;
	void *frame_stack_bottom;
	void *current_handler;
	jmp_buf *exn_jmpbuf;          /* longjmp if uncaught exception error */
	sml_obstack_t *tmp_root;      /* temporary root slots of GC. */
	struct sml_control *prev, *next;  /* for double-linked list */
};

static struct sml_control *global_control;

static void control_finalize();

SML_PRIMITIVE void
sml_control_start(void *frame_pointer)
{
	if (global_control == NULL) {
		global_control = xmalloc(sizeof(struct sml_control));
		global_control->frame_stack_top = frame_pointer;
		global_control->frame_stack_bottom = frame_pointer;
		global_control->current_handler = NULL;
		global_control->exn_jmpbuf = NULL;
		global_control->tmp_root = NULL;
		DBG(("START NEW THREAD : %p", pthread_self()));
	} else {
		FRAME_HEADER(frame_pointer) |= FRAME_FLAG_SKIP;
		FRAME_EXTRA(frame_pointer) =
			(uintptr_t)global_control->frame_stack_top;
		global_control->frame_stack_top = frame_pointer;
	}

}

static void
control_finalize()
{
	if (global_control == NULL)
		return;
	sml_obstack_free(&global_control->tmp_root, NULL);
	free(global_control);
	global_control = NULL;
}

SML_PRIMITIVE void
sml_control_finish(void *frame_pointer)
{
	if (global_control->frame_stack_bottom == frame_pointer) {
		DBG(("FINISH THREAD : %p", pthread_self()));
		control_finalize();
	} else {
		ASSERT(FRAME_HEADER(frame_pointer) & FRAME_FLAG_SKIP);
		global_control->frame_stack_top = (void*)FRAME_EXTRA(frame_pointer);
	}
}

/*
 * prepares new "num_slots" pointer slots which are part of root set of garbage
 * collection, and returns the address of array of the new pointer slots.
 * These pointer slots are available until sml_pop_tmp_rootset() is called.
 * Returned address is only available in the same thread.
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
 * releases last pointer slots allocated by sml_push_tmp_rootset()
 * in the same thread.
 */
void
sml_pop_tmp_rootset(void **slots)
{
	sml_obstack_free(&global_control->tmp_root, slots);
}

SML_PRIMITIVE void
sml_save_frame_pointer(void *p)
{
	global_control->frame_stack_top = p;
}

void *
sml_load_frame_pointer()
{
	return global_control->frame_stack_top;
}

SML_PRIMITIVE void
sml_push_handler(void *handler)
{
	/* The detail of structure of handler is platform-dependent except
	 * that runtime may use *(void**)handler for handler chain. */

	*((void**)handler) = global_control->current_handler;

	/* assume that this assignment is atomic so that asynchronous signal
	 * may raise an exception. */
	global_control->current_handler = handler;

	/*DBG(("ip=%p from %p", ((void**)handler)[1],
	  __builtin_return_address(0)));*/
}

SML_PRIMITIVE void *
sml_pop_handler(void *exn)
{
	void *handler = global_control->current_handler;
	void *prev;
	jmp_buf *buf;

	if (handler == NULL) {
		/* uncaught exception */
		buf = global_control->exn_jmpbuf;
		control_finalize();
		if (buf) {
			longjmp(*buf, 1);
		} else {
			sml_error(0, "uncaught exception: %s",
				  sml_exn_name(exn));
			abort();
		}
	}

	prev = *((void**)handler);

	/* assume that this assignment is atomic so that asynchronous signal
	 * may raise an exception. */
	global_control->current_handler = prev;

	/*DBG(("ip=%p from %p", ((void**)handler)[1],
	  __builtin_return_address (0)));*/

	return handler;
}

static void
frame_enum_ptr(void *frame_info, void (*trace)(void **))
{
	void **boxed;
	unsigned int *sizes, *bitmaps, num_generics, num_boxed;
	unsigned int i, j, num_slots;
	ptrdiff_t offset;
	char *generic;

	num_boxed = FRAME_NUM_BOXED(frame_info);
	num_generics = FRAME_NUM_GENERIC(frame_info);
	boxed = FRAME_BOXED_PART(frame_info);

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
		} else {
			for (j = 0; j < num_slots; j++) {
				generic -= SIZEOF_GENERIC;
				trace((void**)generic);
			}
		}
	}
}

static void
stack_enum_ptr(void (*trace)(void **),
	       void *frame_stack_top, void *frame_stack_bottom)
{
	void *fp = frame_stack_top;
	uintptr_t header;
	intptr_t offset;

	for (;;) {
		header = FRAME_HEADER(fp);
		FRAME_HEADER(fp) = header | FRAME_FLAG_VISITED;

		offset = FRAME_INFO_OFFSET(header);
		if (offset != 0)
			frame_enum_ptr((char*)fp + offset, trace);

		/* When MINOR tracing, we need to trace not only unvisited
		 * frames but also the first frame of visited frames since
		 * the first frame may be modified by ML code from the
		 * previous frame tracing.
		 */

		if (fp == frame_stack_bottom)
			break;

		if (header & FRAME_FLAG_SKIP)
			fp = (void*)FRAME_EXTRA(fp);
		else
			fp = FRAME_NEXT(fp);
	}

	DBG(("frame end"));
}

int
sml_protect(void (*func)(void *), void *data)
{
	jmp_buf *prev, buf;
	int ret, need_finish = 0;
	void *dummy_frame[3];

	if (global_control == NULL) {
		FRAME_HEADER(&dummy_frame[1]) = 0;
		sml_control_start(&dummy_frame[1]);
		need_finish = 1;
	}

	prev = global_control->exn_jmpbuf;
	global_control->exn_jmpbuf = &buf;
	ret = setjmp(buf);
	if (ret == 0)
		func(data);
	global_control->exn_jmpbuf = prev;

	if (need_finish)
		sml_control_finish(&dummy_frame[1]);

	return ret;
}

struct enum_ptr_cls {
	void (*trace)(void **);
};

static void
tmp_root_enum_ptr(void *start, void *end, void *data)
{
	const struct enum_ptr_cls *cls = data;
	void (*trace)(void **) = cls->trace;
	void **i;
	for (i = start; i < (void**)end; i++)
		trace(i);
}

void
sml_control_enum_ptr(void (*trace)(void **))
{
	struct sml_control *control = global_control;
	struct enum_ptr_cls arg = {trace};

	stack_enum_ptr(trace, control->frame_stack_top,
		       control->frame_stack_bottom);
	sml_obstack_enum_chunk(control->tmp_root,
			       tmp_root_enum_ptr, &arg);
}

/* for debug */
void
sml_control_dump()
{
	struct sml_control *control = global_control;

		sml_notice("%p: stack=(%p, %p)"
#ifdef DEBUG
			   " lock_at=%s"
#endif /* DEBUG */
			   , control,
			   control->frame_stack_top,
			   control->frame_stack_bottom
			   );
}
