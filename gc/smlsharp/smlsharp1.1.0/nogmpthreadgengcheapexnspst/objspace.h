/*
 * objspace.h
 * @copyright (c) 2010, Tohoku University.
 * @author UENO Katsuhiro
 * @version $Id: value.c,v 1.5 2008/02/05 08:54:35 katsu Exp $
 */
#ifndef SMLSHARP__OBJSPACE_H__
#define SMLSHARP__OBJSPACE_H__

void sml_objspace_dump();

/*
 * Forcely start garbage collection.
 */
void sml_heap_gc(void);

/*
 * allocate an arbitrary heap object.
 */
SML_PRIMITIVE void *sml_alloc(unsigned int objsize, void *frame_pointer);

SML_PRIMITIVE void sml_global_barrier(void **writeaddr, void *obj);

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
