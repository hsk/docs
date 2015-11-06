/**
 * obstack.c - naive obstack implementation
 * @copyright (c) 2007-2009, Tohoku University.
 * @author UENO Katsuhiro
 * @version $Id: $
 */

#include <stdlib.h>
#include <stdio.h>
#include "smlsharp.h"

void *
xmalloc(size_t size)
{
        void *p = malloc(size);
        if (p == NULL)
                sml_sysfatal("malloc");
        return p;
}

void *
xrealloc(void *p, size_t size)
{
        p = realloc(p, size);
        if (p == NULL)
                sml_sysfatal("realloc");
        return p;
}

struct sml_obstack {
	struct sml_obstack *next;
	char *start, *end;
	char *free, *base;
};

/* assume that libc consumes 8 bytes for memory management. */
#define MINCHUNKSIZE  248

static void
chunk_alloc(sml_obstack_t **obstack, size_t objsize)
{
	size_t head_size, chunk_size;
	sml_obstack_t *newchunk;

	head_size = ALIGNSIZE(sizeof(struct sml_obstack), objsize);
	chunk_size = objsize + head_size;
	chunk_size = ALIGNSIZE(chunk_size, MINCHUNKSIZE);
	newchunk = xmalloc(chunk_size);
	newchunk->next = *obstack;
	newchunk->start = (char*)newchunk + head_size;
	newchunk->end = (char*)newchunk + chunk_size;
	newchunk->free = newchunk->start;
	newchunk->base = newchunk->start;
	*obstack = newchunk;
}

static void
chunk_realloc(sml_obstack_t **obstack, size_t inc)
{
	size_t head_size, chunk_size, free_offset, base_offset;
	sml_obstack_t *chunk = *obstack;
	sml_obstack_t *newchunk;

	head_size = chunk->start - (char*)chunk;
	chunk_size = head_size + (chunk->free - chunk->start) + inc;
	chunk_size = ALIGNSIZE(chunk_size, MINCHUNKSIZE);
	free_offset = chunk->free - (char*)chunk;
	base_offset = chunk->base - (char*)chunk;
	newchunk = xrealloc(chunk, chunk_size);
	newchunk->start = (char*)newchunk + head_size;
	newchunk->end = (char*)newchunk + chunk_size;
	newchunk->free = (char*)newchunk + free_offset;
	newchunk->base = (char*)newchunk + base_offset;
	*obstack = newchunk;
}

void
sml_obstack_blank(sml_obstack_t **obstack, size_t size)
{
	sml_obstack_t *chunk = *obstack;

	if (chunk == NULL || (size_t)(chunk->end - chunk->free) < size) {
		if (chunk == NULL || chunk->base == chunk->free)
			chunk_alloc(obstack, size);
		else
			chunk_realloc(obstack, size);
		chunk = *obstack;
	}
	chunk->free += size;
}

void *
sml_obstack_extend(sml_obstack_t **obstack, size_t size)
{
	sml_obstack_blank(obstack, size);
	return (*obstack)->free - size;
}

void
sml_obstack_shrink(sml_obstack_t **obstack, void *p)
{
	ASSERT((*obstack)->base <= (char*)p && (char*)p <= (*obstack)->free);
	(*obstack)->free = p;

	if ((*obstack)->end - (*obstack)->free > MINCHUNKSIZE)
		chunk_realloc(obstack, 0);
}

void *
sml_obstack_finish(sml_obstack_t *obstack)
{
	void *dst = obstack->base;
	obstack->base = obstack->free;
	return dst;
}

void *
sml_obstack_base(sml_obstack_t *obstack)
{
	return obstack->base;
}

void *
sml_obstack_next_free(sml_obstack_t *obstack)
{
	return obstack->free;
}

size_t
sml_obstack_object_size(sml_obstack_t *obstack)
{
	return obstack->free - obstack->base;
}

void
sml_obstack_align(sml_obstack_t **obstack, size_t size)
{
	size_t offset;

	if (*obstack) {
		offset = (*obstack)->base - (char*)(*obstack);
		sml_obstack_blank(obstack, ALIGNSIZE(offset, size) - offset);
		(*obstack)->base = (*obstack)->free;
	}
}

void *
sml_obstack_alloc(sml_obstack_t **obstack, size_t size)
{
	sml_obstack_blank(obstack, size);
	return sml_obstack_finish(*obstack);
}

/*
 * ptr must be either NULL or the address of an object allocated in
 * the obstack.
 * If ptr is NULL, the whole of obstack is freed.
 * Otherwise, every object allocated in objstack since ptr is freed.
 */
void
sml_obstack_free(sml_obstack_t **obstack, void *ptr)
{
	struct sml_obstack *chunk, *next;

	chunk = *obstack;

	while (chunk) {
		if (chunk->start <= (char*)ptr
		    && (char*)ptr <= chunk->free) {
			chunk->free = ptr;
			chunk->base = ptr;
			*obstack = chunk;
			return;
		}
		next = chunk->next;
		free(chunk);
		chunk = next;
	}

	if (ptr != NULL)
		sml_fatal(0, "BUG: obstack_free: invalid pointer: %p %p",
			  (void*)*obstack, ptr);

	*obstack = NULL;
}

void
sml_obstack_enum_chunk(sml_obstack_t *obstack,
		       void (*enumfunc)(void *start, void *end, void *data),
		       void *data)
{
	while (obstack) {
		enumfunc(obstack->start, obstack->free, data);
		obstack = obstack->next;
	}
}

int
sml_obstack_is_empty(sml_obstack_t *obstack)
{
	return (obstack->start == obstack->base
		&& obstack->base == obstack->free
		&& obstack->next == NULL);
}
