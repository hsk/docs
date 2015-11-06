/*
 * objspace.c - common heap management features.
 * @copyright (c) 2010, Tohoku University.
 * @author UENO Katsuhiro
 * @version $Id: value.c,v 1.5 2008/02/05 08:54:35 katsu Exp $
 */

#include <stdlib.h>
#include <stdint.h>
#include "smlsharp.h"
#include "object.h"
#include "control.h"
#include "objspace.h"
#include "splay.h"

/* tree node allocator for persistent trees. */
static sml_obstack_t *persistent_node_obstack = NULL;
static void *persistent_node_alloc(size_t size);

/* barriered slot */
static int voidp_cmp(void *, void *);
static sml_tree_t global_barrier =
	SML_TREE_INITIALIZER(voidp_cmp, persistent_node_alloc, NULL);

/* callback closures */
static int callback_cmp(void *, void *);
static sml_tree_t callback_closures =
	SML_TREE_INITIALIZER(callback_cmp, persistent_node_alloc, NULL);

struct callback_item {
	void *closure;
	void *entry;
	void *env;    /* ML object. global_barrier keeps track here. */
	struct callback_item *next;
};

/* malloc heap */

static sml_tree_t malloc_heap =
	SML_TREE_INITIALIZER(voidp_cmp, xmalloc, free);
static size_t malloc_count;

struct malloc_obj_header {
	struct malloc_obj_header *next;  /* next object in the stack */
	unsigned int flags;
};

#define MALLOC_FLAG_REMEMBER  0x1
#define MALLOC_FLAG_TRACED    0x2

/* top of mark stack.
 * mark stark is used not only for collection but also remembered set for
 * the next minor collection.
 */
static struct malloc_obj_header *malloc_stack_top = NULL;

#define MALLOC_PADDING \
	ALIGNSIZE(sizeof(struct malloc_obj_header) + OBJ_HEADER_SIZE, MAXALIGN)
#define MALLOC_HEAD(objptr) \
	((struct malloc_obj_header*)((char*)(objptr) - MALLOC_PADDING))
#define MALLOC_BODY(headptr) \
	((void*)((char*)(headptr) + MALLOC_PADDING))
#define MALLOC_LIMIT  (1024 * 1024 * 4)

static void *
persistent_node_alloc(size_t size)
{
	return sml_obstack_alloc(&persistent_node_obstack, size);
}

static void
dump_malloc(void *item, void *data ATTR_UNUSED)
{
	sml_notice("%p (flags=%08x, size=%lu)", item,
		   MALLOC_HEAD(item)->flags, (unsigned long)OBJ_SIZE(item));
}

static void
dump_callback(void *item, void *data ATTR_UNUSED)
{
	struct callback_item *cls = item;
	while (cls) {
		sml_notice("closure=%p, entry=%p, env=%p",
			   cls->closure, cls->entry, cls->env);
		cls = cls->next;
	}
}

static void
dump_barrier(void *item, void *data ATTR_UNUSED)
{
	sml_notice("%p -> %p", item, *(void**)item);
}

/* for debug */
void
sml_objspace_dump()
{
	struct malloc_obj_header *p;
	sml_notice("malloc :");
	sml_tree_each(&malloc_heap, dump_malloc, NULL);
	sml_notice("callbacks :");
	sml_tree_each(&callback_closures, dump_callback, NULL);
	sml_notice("barriered :");
	sml_tree_each(&global_barrier, dump_barrier, NULL);
	sml_notice("mark stack :");
	for (p = malloc_stack_top; p; p = p->next)
		dump_malloc(MALLOC_BODY(p), NULL);
}

static int
voidp_cmp(void *x, void *y)
{
	uintptr_t m = (uintptr_t)x, n = (uintptr_t)y;
	if (m < n) return -1;
	else if (m > n) return 1;
	else return 0;
}


void
sml_finish(void)
{
	callback_closures.root = NULL;
	global_barrier.root = NULL;
	sml_obstack_free(&persistent_node_obstack, NULL);
}

/* malloc heap */

void *
sml_obj_malloc(size_t objsize)
{
	/* objsize = payload_size + bitmap_size */
	struct malloc_obj_header *head;
	void *obj;
	size_t alloc_size;

	if (malloc_count > MALLOC_LIMIT) {
		sml_heap_gc();
	}

	alloc_size = MALLOC_PADDING + objsize;
	head = xmalloc(alloc_size);
	obj = MALLOC_BODY(head);
	sml_tree_insert(&malloc_heap, obj);
	malloc_count += alloc_size;

	/* if the new object is an immutable object such as record, ML
	 * object pointer may be stored without write barrier during
	 * initialization of the new object. */
	head->next = malloc_stack_top;
	malloc_stack_top = head;
	head->flags = MALLOC_FLAG_REMEMBER;

	OBJ_HEADER(obj) = 0;
	return obj;
}

void
sml_trace_ptr(void *obj)
{

	if (sml_tree_find(&malloc_heap, obj)) {
		struct malloc_obj_header *head = MALLOC_HEAD(obj);
		ASSERT(head->flags != 0 || head->next == NULL);
		if (head->flags == 0) {
			head->next = malloc_stack_top;
			malloc_stack_top = head;
		}
		head->flags |= MALLOC_FLAG_TRACED;
	}
}

static void
malloc_barrier(void *obj)
{
	struct malloc_obj_header *head = MALLOC_HEAD(obj);


	ASSERT(head->flags == MALLOC_FLAG_REMEMBER
	       || (head->flags == 0 && head->next == NULL));
	if (head->flags == 0) {
		head->flags |= MALLOC_FLAG_REMEMBER;
		head->next = malloc_stack_top;
		malloc_stack_top = head;
	}
}

void
sml_malloc_pop_and_mark(void (*trace)(void **))
{
	struct malloc_obj_header *head;

	/* check only traced objects */
	while (malloc_stack_top) {
		head = malloc_stack_top;
		malloc_stack_top = malloc_stack_top->next;
		head->next = NULL;
		if (head->flags & MALLOC_FLAG_TRACED)
			sml_obj_enum_ptr(MALLOC_BODY(head), trace);
		else
			head->flags = 0;
	}
}

static int
malloc_heap_sweep(void *item)
{
	void **obj = item;
	struct malloc_obj_header *head = MALLOC_HEAD(obj);

	ASSERT(head->next == NULL);

	if (head->flags == 0) {   /* unmarked */
		free(head);
		return 1;
	} else {
		head->flags = 0;  /* clear mark */
		return 0;
	}
}

void
sml_malloc_sweep()
{
	ASSERT(malloc_stack_top == NULL);

	sml_tree_reject(&malloc_heap, malloc_heap_sweep);
	malloc_count = 0;
}

/* root set management */

static void
each_barrier(void *item, void *data)
{
	void (**trace)(void **) = data;
	void **addr = item;
	(*trace)(addr);
}

void
sml_rootset_enum_ptr(void (*trace)(void **))
{

	/* global_barrier includes every addresses in
	 * callback_closures where holds an ML object. */
	sml_tree_each(&global_barrier, each_barrier, &trace);
	sml_control_enum_ptr(trace);
}

/* global barrier */

void
sml_global_barrier(void **writeaddr, void *obj)
{

	/* check whether obj is in malloc heap. */
	if (sml_tree_find(&malloc_heap, obj)) {
		malloc_barrier(obj);
	} else {
		/* There is a reference to an ML object from outside.
		 * remember the writeaddr as a root set. */
		sml_tree_insert(&global_barrier, writeaddr);
	}

}

/**** callback closures ****/

static int
callback_cmp(void *x, void *y)
{
	struct callback_item *item1 = x, *item2 = y;
	return voidp_cmp(item1->entry, item2->entry);
}

SML_PRIMITIVE void *
sml_alloc_callback(unsigned int objsize, void *codeaddr, void *envobj)
{
	/* NOTE: returned object may be stored an ML object pointer
	 * without write barrier. sml_alloc_callback need to add
	 * the resulting object to remembered set so that such an
	 * ML pointer may be traced by the next minor collection. */

	struct callback_item key, *item, *prev;

	key.entry = codeaddr;

	prev = sml_tree_find(&callback_closures, &key);
	if (prev != NULL) {
		/* if a same closure is already created, return it. */
		for (item = prev; item; item = item->next) {
			if (sml_obj_equal(envobj, item->env)) {
				malloc_barrier(item->closure);
				return item->closure;
			}
		}
	}

	/* using persistent_node_alloc is safe under the assumption that
	 * sizeof(struct callback_item) == sizeof(struct sml_tree_node). */
	item = persistent_node_alloc(sizeof(struct callback_item));

	item->entry = codeaddr;
	item->next = prev;
	item->env = envobj;
	item->closure = NULL;
 	sml_tree_insert(&callback_closures, item);

	/* remember callback item as a part of root set. */
	sml_tree_insert(&global_barrier, &item->env);
	sml_tree_insert(&global_barrier, &item->closure);

	/* assume that malloc returns memory with executable permission. */
	item->closure = sml_obj_malloc(objsize);

	return item->closure;
}

static void
trace(void **slot)
{
	sml_trace_ptr(*slot);
}

void
sml_heap_gc()
{
	sml_rootset_enum_ptr(trace);
	sml_malloc_pop_and_mark(trace);
	sml_malloc_sweep();
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
