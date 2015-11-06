/*
 * objspace.h
 * @copyright (c) 2010, Tohoku University.
 * @author UENO Katsuhiro
 * @version $Id: value.c,v 1.5 2008/02/05 08:54:35 katsu Exp $
 */
#ifndef SMLSHARP__OBJSPACE_H__
#define SMLSHARP__OBJSPACE_H__

/*
 * enumerate pointer slots in root set.
 */
void sml_rootset_enum_ptr(void (*callback)(void **));

/*
 * allocate an ML object by malloc.
 * malloc'ed objects are managed by mark-and-sweep collector.
 * objsize: allocation size except object header in bytes.
 */
void *sml_obj_malloc(size_t objsize);

/*
 * write barrier for global memory.
 * writeaddr : write address.
 * objaddr : address of object including writeaddr.
 *
 * Write barrier must call this function if writeadr is not in heap.
 */
void sml_global_barrier(void **writeaddr, void *objaddr);

/*
 * trace pointer which is outside of heap.
 * ptr: pointer to be traced.
 *
 * Garbage collector must call this function when it meets an ML object
 * pointer at outside of its heap.
 * If ptr is NULL, garbage collector is not needed to call this function.
 */
void sml_trace_ptr(void *ptr);

/*
 * pop and mark malloc'ed objects until mark stack of malloc heap becomes
 * empty.
 *
 * Garbage collector must call this function at tracing phase.
 * Before leaving tracing phase, make sure that the mark stack of malloc heap
 * is empty.
 */
void sml_malloc_pop_and_mark(void (*trace)(void **));

/*
 * sweep malloc'ed objects.
 *
 * Garbage collector must call this function at collection phase.
 */
void sml_malloc_sweep();

/*
 * Forcely start garbage collection.
 */
void sml_heap_gc(void);

/*
 * allocate an arbitrary heap object.
 */
SML_PRIMITIVE void *sml_alloc(unsigned int objsize, void *frame_pointer);

/*
 * update a pointer field of "obj" indicated by "writeaddr" with "new_value".
 * The heap implementation may perform additional tasks to keep track of
 * pointer updates.
 *
 * This function will be called with objects at both inside and outside
 * of heap. If "writeaddr" and/or "objaddr" is not in any heap, heap
 * implementation must call sml_global_barrier after update.
 */
SML_PRIMITIVE void sml_write(void *obj, void **writeaddr, void *new_value);

#endif /* SMLSHARP__OBJSPACE_H__ */
