/*
 * heap_bitmap.c
 * @copyright (c) 2010, Tohoku University.
 * @author UENO Katsuhiro
 * @author Yudai Asai
 * @version $Id: $
 */

#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>

#include <sys/mman.h>

#include "smlsharp.h"
#include "object.h"
#include "objspace.h"
#include "heap.h"

/*#define SURVIVAL_CHECK*/
/*#define GCSTAT*/
/*#define GCTIME*/
/*#define NULL_IS_NOT_ZERO*/
/*#define MINOR_GC*/
/*#define DEBUG_USE_MMAP */


/* bit pointer */
struct bitptr {
	unsigned int *ptr;
	unsigned int mask;
};
typedef struct bitptr bitptr_t;

#define BITPTR_WORDBITS  ((unsigned int)(sizeof(unsigned int) * CHAR_BIT))

#define BITPTR_INIT(b,p,n) \
	((b).ptr = (p) + (n) / BITPTR_WORDBITS, \
	 (b).mask = 1 << ((n) % BITPTR_WORDBITS))
#define BITPTR_TEST(b)  (*(b).ptr & (b).mask)
#define BITPTR_SET(b)   (*(b).ptr |= (b).mask)
#define BITPTR_CLEAR(b) (*(b).ptr &= ~(b).mask)
#define BITPTR_WORD(b)  (*(b).ptr)
#define BITPTR_WORDINDEX(b,p)  ((b).ptr - (p))
#define BITPTR_EQUAL(b1,b2) \
	((b1).ptr == (b2).ptr && (b1).mask == (b2).mask)

/* BITPTR_NEXT: find 0 bit in current word after and including
 * pointed bit. */
#define BITPTR_NEXT(b) do {				 \
	unsigned int tmp__ = *(b).ptr | ((b).mask - 1U); \
	(b).mask = (tmp__ + 1U) & ~tmp__;		 \
} while (0)
#define BITPTR_NEXT_FAILED(b)  ((b).mask == 0)

static bitptr_t
bitptr_linear_search(unsigned int *start, const unsigned int *limit)
{
	bitptr_t b = {start, 0};
	while (b.ptr < limit) {
		b.mask = (*b.ptr + 1) & ~*b.ptr;
		if (b.mask) break;
		b.ptr++;
	}
	return b;
}

#if defined(__GNUC__) && !defined(NOASM) && defined(HOST_CPU_i386)
#define BITPTR_INC(b) do {						\
	unsigned int tmp__;						\
	__asm__ ("xorl\t%0, %0\n\t"					\
		 "roll\t%1\n\t"						\
		 "rcll\t%0"						\
		 : "=&r" (tmp__), "+r" ((b).mask));			\
	(b).ptr += tmp__;						\
} while (0)
#else
#define BITPTR_INC(b) \
	(((b).mask <<= 1) ? (void)0 : (void)((b).mask = 1, (b).ptr++))
#endif /* !NOASM */

#if defined(__GNUC__) && !defined(NOASM) && defined(HOST_CPU_i386)
#define bsr(x) ({							\
	unsigned int tmp__;						\
	ASSERT((x) > 0);						\
	__asm__ ("bsrl\t%1, %0" : "=r" (tmp__) : "r" ((unsigned int)(x))); \
	tmp__;								\
})
#define bsf(x) ({							\
	unsigned int tmp__;						\
	ASSERT((x) > 0);						\
	__asm__ ("bsfl\t%1, %0" : "=r" (tmp__) : "r" ((unsigned int)(x))); \
	tmp__;								\
})
#elif defined(SIZEOF_INT) && (SIZEOF_INT == 4)
static inline unsigned int
bsr(unsigned int m)
{
	unsigned int x, n = 0;
	ASSERT(m > 0);
	x = m >> 16; if (x != 0) n += 16, m = x;
	x = m >> 8; if (x != 0) n += 8, m = x;
	x = m >> 4; if (x != 0) n += 4, m = x;
	x = m >> 2; if (x != 0) n += 2, m = x;
	return n + (m >> 1);
}
static inline unsigned int
bsf(unsigned int m)
{
	unsigned int x, n = 31;
	ASSERT(m > 0);
	x = m << 16; if (x != 0) n -= 16, m = x;
	x = m << 8; if (x != 0) n -= 8, m = x;
	x = m << 4; if (x != 0) n -= 4, m = x;
	x = m << 2; if (x != 0) n -= 2, m = x;
	x = m << 1; if (x != 0) n -= 1;
	return n;
}
#else
static inline unsigned int
bsr(unsigned int m)
{
	unsigned int x, n = 0, c = BITPTR_WORDBITS / 2;
	ASSERT(m > 0);
	do {
		x = m >> c; if (x != 0) n += c, m = x;
		c >>= 1;
	} while (c > 1);
	return n + (m >> 1);
}
static inline unsigned int
bsf(unsigned int m)
{
	unsigned int x, n = 31, c = BITPTR_WORDBITS / 2;
	ASSERT(m > 0);
	do {
		x = m << c; if (x != 0) n -= c, m = x;
		c >>= 1;
	} while (c > 0);
	return n;
}
#endif /* NOASM */

/* BITPTR_INDEX: bit index of 'b' counting from first bit of 'base'. */
#define BITPTR_INDEX(b,p) \
	(((b).ptr - (p)) * BITPTR_WORDBITS + bsf((b).mask))

#define CEIL_LOG2(x) \
	(bsr((x) - 1) + 1)

/* segments */

#ifndef SEGMENT_SIZE
#define SEGMENT_SIZE  131072  /* 128k */
#endif /* SEGMENT_SIZE */
#ifndef SEG_RANK
#define SEG_RANK  3
#endif /* SEG_RANK */

#define BLOCKSIZE_MIN_LOG2  3U   /* 2^3 = 8 */
#define BLOCKSIZE_MIN       (1U << BLOCKSIZE_MIN_LOG2)
#define BLOCKSIZE_MAX_LOG2  12U  /* 2^4 = 16 */
#define BLOCKSIZE_MAX       (1U << BLOCKSIZE_MAX_LOG2)

struct segment_layout {
	size_t blocksize;
	size_t bitmap_offset[SEG_RANK];
	size_t bitmap_limit[SEG_RANK];
	unsigned int bitmap_sentinel[SEG_RANK];
	size_t bitmap_size;
	size_t stack_offset;
	size_t stack_limit;
	size_t block_offset;
	size_t num_blocks;
};

struct segment {
	struct segment *next;
	unsigned int live_count;
	struct stack_slot { void *next_obj; } *stack;
	char *block_base;
	const struct segment_layout *layout;
	unsigned int blocksize_log2;
};

/*
 * segment layout:
 *
 * 00000 +--------------------------+
 *       | struct segment           |
 *       +--------------------------+ SEG_BITMAP_BASE0 (aligned in MAXALIGN)
 *       | bitmap(0)                | ^
 *       :                          : | about N bits + sentinel
 *       |                          | V
 *       +--------------------------+ SEG_BITMAP_BASE1
 *       | bitmap(1)                | ^
 *       :                          : | about N/32 bits + sentinel
 *       |                          | V
 *       +--------------------------+ SEG_BITMAP_BASE2
 *       :                          :
 *       +--------------------------+ SEG_BITMAP_BASEn
 *       | bitmap(n)                | about N/32^n bits + sentinel
 *       |                          |
 *       +--------------------------+ SEG_STACK_BASE
 *       | stack area               | ^
 *       |                          | | N pointers
 *       |                          | v
 *       +--------------------------+ SEG_BLOCK_BASE (aligned in MAXALIGN)
 *       | obj block area           | ^
 *       |                          | | N blocks
 *       |                          | v
 *       +--------------------------+
 *       :                          :
 * 80000 +--------------------------+
 *
 * N-th bit of bitmap(0) indicates whether N-th block is used (1) or not (0).
 * N-th bit of bitmap(n) indicates whether N-th word of bitmap(n-1) is
 * filled (1) or not (0).
 */

#define WORDBITS BITPTR_WORDBITS

#define CEIL_(x,y)         ((((x) + (y) - 1) / (y)) * (y))
#define BITS_TO_WORDS_(n)  (((n) + WORDBITS - 1) / WORDBITS)
#define WORDS_TO_BYTES_(n) ((n) * sizeof(unsigned int))

#define APPLY_(f,a)    f a
#define REPEAT_0(f,z)  z
#define REPEAT_1(f,z)  APPLY_(f, REPEAT_0(f,z))
#define REPEAT_2(f,z)  APPLY_(f, REPEAT_1(f,z))
#define REPEAT_3(f,z)  APPLY_(f, REPEAT_2(f,z))
#define REPEAT_4(f,z)  APPLY_(f, REPEAT_3(f,z))
#define REPEAT_(i,f,z) REPEAT__(i,f,z)
#define REPEAT__(i,f,z) REPEAT_##i(f,z)

#define INC_0    1
#define INC_1    2
#define INC_2    3
#define INC_3    4
#define INC_4    5
#define INC__(n) INC_##n
#define INC_(n)  INC__(n)

#define LIST_1(f,n)   f(0,n)
#define LIST_2(f,n)   LIST_1(f,n), f(1,n)
#define LIST_3(f,n)   LIST_2(f,n), f(2,n)
#define LIST_4(f,n)   LIST_3(f,n), f(3,n)
#define LIST_5(f,n)   LIST_4(f,n), f(4,n)
#define LIST_(i,f,n)  LIST_##i(f,n)
#define ARRAY_(i,f,n) {LIST_(i,f,n)}
#define DUMMY_(x,y)   y

#define SEL1_(a,b,c)  a
#define SEL2_(a,b,c)  b
#define SEL3_(a,b,c)  c

#define SEG_INITIAL_OFFSET CEIL_(sizeof(struct segment), MAXALIGN)

#define SEG_BITMAP0_OFFSET   SEG_INITIAL_OFFSET
#define SEG_BITMAP_BOTTOM(n)					\
	(/* offset */ SEG_BITMAP0_OFFSET,			\
	 /* bits */   n,					\
	 /* words */  BITS_TO_WORDS_((n) + 1))
#define SEG_BITMAP_ITERATE(offset,bits,words)			\
	(/* offset */ (offset) + WORDS_TO_BYTES_(words),	\
	 /* bits */   BITS_TO_WORDS_(bits),			\
	 /* words */  BITS_TO_WORDS_((words) + 1))

#define SEG_BITMAP_(i,n) \
	REPEAT_(i, SEG_BITMAP_ITERATE, SEG_BITMAP_BOTTOM(n))

#define SEG_BITMAP_OFFSET_(i,n) (APPLY_(SEL1_,SEG_BITMAP_(i,n)))
#define SEG_BITMAP_BITS_(i,n)   (APPLY_(SEL2_,SEG_BITMAP_(i,n)))
#define SEG_BITMAP_WORDS_(i,n)  (APPLY_(SEL3_,SEG_BITMAP_(i,n)))
#define SEG_BITMAP_SIZE_(i,n)   WORDS_TO_BYTES(SEG_BITMAP_WORDS_(i,n))
#define SEG_BITMAP_LIMIT_(i,n)  SEG_BITMAP_OFFSET_(INC_(i), n)
#define SEG_BITMAP_SENTINEL_BITS_(i,n) \
	(SEG_BITMAP_WORDS_(i,n) * WORDBITS - SEG_BITMAP_BITS_(i,n))

#define SEG_STACK_OFFSET_(n) \
	CEIL_(SEG_BITMAP_OFFSET_(SEG_RANK, n), sizeof(struct stack_slot))
#define SEG_STACK_SIZE_(n) \
	((n) * sizeof(struct stack_slot))
#define SEG_STACK_LIMIT_(n) \
	(SEG_STACK_OFFSET_(n) + SEG_STACK_SIZE_(n))
#define SEG_BLOCK_OFFSET_(n,s) \
	CEIL_(SEG_STACK_LIMIT_(n) + OBJ_HEADER_SIZE, MAXALIGN)
#define SEG_TOTAL_SIZE_(numblocks,blocksize) \
	(SEG_BLOCK_OFFSET_(numblocks,blocksize) + (numblocks) * (blocksize))

#define SEG_NUM_BLOCKS_ESTIMATE(blocksize)		\
	((size_t)(((double)SEGMENT_SIZE			\
		   - (double)SEG_INITIAL_OFFSET		\
		   - (double)SEG_RANK / CHAR_BIT)	\
		  / ((double)(blocksize)		\
		     + (1.f				\
			+ 1.f / WORDBITS		\
			+ 1.f / WORDBITS / WORDBITS)	\
		     / CHAR_BIT				\
		     + sizeof(void*))))

#define SEG_OVERFLOW_BYTES(blocksize) \
	((signed)(SEG_TOTAL_SIZE_(SEG_NUM_BLOCKS_ESTIMATE(blocksize),	\
				  blocksize) - SEGMENT_SIZE))
#define SEG_OVERFLOW_BLOCKS(blocksize) \
	((SEG_OVERFLOW_BYTES(blocksize) + (blocksize) - 1) \
	 / (signed)(blocksize))
#define SEG_NUM_BLOCKS(blocksize) \
	(SEG_NUM_BLOCKS_ESTIMATE(blocksize) - SEG_OVERFLOW_BLOCKS(blocksize))

#define SEG_BITMAP_OFFSET(level, blocksize) \
	SEG_BITMAP_OFFSET_(level, SEG_NUM_BLOCKS(blocksize))
#define SEG_BITMAP_LIMIT(level, blocksize) \
	SEG_BITMAP_LIMIT_(level, SEG_NUM_BLOCKS(blocksize))
#define SEG_BITMAP_SIZE(blocksize) \
	/* aligning in MAXALIGN makes memset faster. \
	 * It is safe since stack area is bigger than MAXALIGN and \
	 * memset never reach both object header and content. */ \
	CEIL_(SEG_BITMAP_OFFSET(SEG_RANK, blocksize) - SEG_BITMAP0_OFFSET, \
	      MAXALIGN)
#define SEG_BITMAP_SENTINEL_BITS(level, blocksize) \
	SEG_BITMAP_SENTINEL_BITS_(level, SEG_NUM_BLOCKS(blocksize))
#define SEG_BITMAP_SENTINEL(level, blocksize) \
	(~0U << (WORDBITS - SEG_BITMAP_SENTINEL_BITS(level, blocksize)))
#define SEG_STACK_OFFSET(blocksize) \
	SEG_STACK_OFFSET_(SEG_NUM_BLOCKS(blocksize))
#define SEG_STACK_SIZE(blocksize) \
	SEG_STACK_SIZE_(SEG_NUM_BLOCKS(blocksize))
#define SEG_STACK_LIMIT(blocksize) \
	SEG_STACK_LIMIT_(SEG_NUM_BLOCKS(blocksize))
#define SEG_BLOCK_OFFSET(blocksize) \
	SEG_BLOCK_OFFSET_(SEG_NUM_BLOCKS(blocksize), blocksize)

#define SEGMENT_LAYOUT(blocksize) \
	{/*blocksize*/       blocksize,\
	 /*bitmap_offset*/   ARRAY_(SEG_RANK, SEG_BITMAP_OFFSET, blocksize), \
	 /*bitmap_limit*/    ARRAY_(SEG_RANK, SEG_BITMAP_LIMIT, blocksize), \
	 /*sentinel*/        ARRAY_(SEG_RANK, SEG_BITMAP_SENTINEL, blocksize),\
	 /*bitmap_size*/     SEG_BITMAP_SIZE(blocksize), \
	 /*stack_offset*/    SEG_STACK_OFFSET(blocksize), \
	 /*stack_limit*/     SEG_STACK_LIMIT(blocksize), \
	 /*block_offset*/    SEG_BLOCK_OFFSET(blocksize), \
	 /*num_blocks*/      SEG_NUM_BLOCKS(blocksize)}

#define SEGMENT_LAYOUT_DUMMY \
	{/*blocksize*/       0, \
	 /*bitmap_offset*/   ARRAY_(SEG_RANK, DUMMY_, SEG_BITMAP0_OFFSET), \
	 /*bitmap_limit*/    ARRAY_(SEG_RANK, DUMMY_, SEGMENT_SIZE), \
	 /*sentinel*/        ARRAY_(SEG_RANK, DUMMY_, 0), \
	 /*bitmap_size*/     SEGMENT_SIZE - SEG_BITMAP0_OFFSET, \
	 /*stack_offset*/    SEGMENT_SIZE, \
	 /*stack_limit*/     SEGMENT_SIZE, \
	 /*block_offset*/    SEGMENT_SIZE, \
	 /*num_blocks*/      0}

const struct segment_layout segment_layout[BLOCKSIZE_MAX_LOG2 + 1] = {
	SEGMENT_LAYOUT_DUMMY,     /* 2^0 = 1 */
	SEGMENT_LAYOUT_DUMMY,     /* 2^1 = 2 */
	SEGMENT_LAYOUT_DUMMY,     /* 2^2 = 4 */
	SEGMENT_LAYOUT(1 << 3),   /* 2^3 = 8 == BLOCKSIZE_MIN  */
	SEGMENT_LAYOUT(1 << 4),   /* 2^4 = 16 */
	SEGMENT_LAYOUT(1 << 5),   /* 2^5 = 32 */
	SEGMENT_LAYOUT(1 << 6),   /* 2^6 = 64 */
	SEGMENT_LAYOUT(1 << 7),   /* 2^7 = 128 */
	SEGMENT_LAYOUT(1 << 8),   /* 2^8 = 256 */
	SEGMENT_LAYOUT(1 << 9),   /* 2^9 = 512 */
	SEGMENT_LAYOUT(1 << 10),  /* 2^10 = 1024 */
	SEGMENT_LAYOUT(1 << 11),  /* 2^11 = 2048 */
	SEGMENT_LAYOUT(1 << 12),  /* 2^12 = 4096 == BLOCKSIZE_MIN */
};

#define ADD_OFFSET(p,n)  ((void*)((char*)(p) + (n)))

#define BITMAP0_BASE(seg) \
	((unsigned int*)ADD_OFFSET(seg, SEG_BITMAP0_OFFSET))
#define BITMAP_BASE(seg, level) \
	((unsigned int*) \
	 ADD_OFFSET(seg, (seg)->layout->bitmap_offset[level]))
#define BITMAP_LIMIT_3(seg, layout, level) \
	((unsigned int*)ADD_OFFSET(seg, (layout)->bitmap_limit[level]))
#define BITMAP_LIMIT(seg, level) \
	BITMAP_LIMIT_3(seg, (seg)->layout, level)
#define BITMAP_SENTINEL(seg, level) \
	((seg)->layout->bitmap_sentinel[level])
#define BLOCK_BASE(seg)  ((seg)->block_base)
#define BLOCK_SIZE(seg)  (1U << (seg)->blocksize_log2)

/* sub heaps */
struct subheap {
	struct segment *seglist;      /* list of segments */
	struct segment **unreserved;  /* head of unreserved segs on seglist */
};

/* allocation pointers */
/*
 * Since allocation pointers are frequently accessed,
 * they should be small so that they can stay in cache as long as possible.
 * And, in order for fast offset computation, sizeof(struct alloc_ptr)
 * should be power of 2.
 */
struct alloc_ptr {
	bitptr_t freebit;
	char *free;
	unsigned int blocksize_bytes;
};

union alloc_ptr_set {
	struct alloc_ptr alloc_ptr[BLOCKSIZE_MAX_LOG2 + 1];
};

static const unsigned int dummy_bitmap = ~0U;
static const bitptr_t dummy_bitptr = { (unsigned int *)&dummy_bitmap, 1 };

static union alloc_ptr_set global_alloc_ptr_set;
#define ALLOC_PTR_SET() (&global_alloc_ptr_set)

struct subheap global_subheaps[BLOCKSIZE_MAX_LOG2 + 1];

static struct {
	struct segment *freelist;
	void *begin, *end;
	unsigned int min_num_segments, max_num_segments, num_committed;
	unsigned int extend_step;
	unsigned int *bitmap;
} heap_space;

#define IS_IN_HEAP(p) \
	((char*)heap_space.begin <= (char*)(p) \
		 && (char*)(p) < (char*)heap_space.end)

#define ALLOC_PTR_TO_BLOCKSIZE_LOG2(ptr)				\
	(ASSERT								\
	 (BLOCKSIZE_MIN_LOG2 <=						\
	  (unsigned)((ptr) - &ALLOC_PTR_SET()->alloc_ptr[0])		\
	  && (unsigned)((ptr) - &ALLOC_PTR_SET()->alloc_ptr[0])		\
	  <= BLOCKSIZE_MAX_LOG2),					\
	 (unsigned)((ptr) - &ALLOC_PTR_SET()->alloc_ptr[0]))

/* bit pointer is suitable for computing segment address.
 * bit pointer always points to the address in the middle of segments. */
#define ALLOC_PTR_TO_SEGMENT(ptr)					\
	(ASSERT(IS_IN_HEAP((ptr)->freebit.ptr)),			\
	 ((struct segment*)                                             \
	  (((uintptr_t)(ptr)->freebit.ptr) & ~(SEGMENT_SIZE - 1U))))

#define OBJ_TO_SEGMENT(objaddr) \
	(ASSERT(IS_IN_HEAP(objaddr)), \
	 ((struct segment*)((uintptr_t)(objaddr) & ~(SEGMENT_SIZE - 1U))))

#define OBJ_TO_INDEX(seg, objaddr)					\
	(ASSERT(OBJ_TO_SEGMENT(objaddr) == (seg)),			\
	 ASSERT((char*)(objaddr) >= (seg)->block_base),			\
	 ASSERT((char*)(objaddr)					\
		< (seg)->block_base + ((seg)->layout->num_blocks	\
				       << (seg)->blocksize_log2)),	\
	 ((size_t)((char*)(objaddr) - (seg)->block_base)		\
	  >> (seg)->blocksize_log2))

/* for debug */
struct segment *obj_to_segment(void *obj) {return OBJ_TO_SEGMENT(obj);}
size_t obj_to_index(void *obj) {
	struct segment *seg = OBJ_TO_SEGMENT(obj);
	return OBJ_TO_INDEX(seg, obj);
}
unsigned int obj_to_bit(void *obj) {
	struct segment *seg = OBJ_TO_SEGMENT(obj);
	size_t index = OBJ_TO_INDEX(seg, obj);
	bitptr_t b;
	BITPTR_INIT(b, BITMAP0_BASE(seg), index);
	return BITPTR_TEST(b);
}
struct stack_slot *obj_to_stack(void *obj) {
	struct segment *seg = OBJ_TO_SEGMENT(obj);
	size_t index = OBJ_TO_INDEX(seg, obj);
	return seg->stack + index;
}


#define UNRESERVED_NEXT(unreserved, seg) do { \
	seg = *unreserved; \
	if (seg) unreserved = &seg->next; \
} while (0)
#define FREELIST_NEXT(freelist, seg) do { \
	seg = freelist;	\
	if (seg) freelist = seg->next; \
} while (0)
#define UNRESERVED_APPEND(unreserved, seg) do { \
	*unreserved = seg; \
	unreserved = &seg->next; \
} while (0)


/* for debug or GCSTAT */
static size_t
segment_filled(struct segment *seg, size_t filled_index, size_t *ret_bytes)
{
	unsigned int i;
	bitptr_t b;
	char *p = BLOCK_BASE(seg);
	size_t filled = 0, count = 0;
	const size_t blocksize = BLOCK_SIZE(seg);

	BITPTR_INIT(b, BITMAP0_BASE(seg), 0);
	for (i = 0; i < seg->layout->num_blocks; i++) {
		if (i < filled_index || BITPTR_TEST(b)) {
			ASSERT(OBJ_TOTAL_SIZE(p) <= blocksize);
			count++;
			filled += OBJ_TOTAL_SIZE(p);
		}
		BITPTR_INC(b);
		p += blocksize;
	}

	if (ret_bytes)
		*ret_bytes = filled;
	return count;
}

/* for debug */
static void
dump_segment_list(struct segment *seg, struct segment *cur)
{
	size_t filled, count;

	while (seg) {
		count = segment_filled(seg, 0, &filled);
		sml_debug("  segment %p:%s\n",
			  seg, seg == cur ? " UNRESERVED" : "");
		sml_debug("    blocksize = %u, "
			  "%lu blocks, %lu blocks used, %lu bytes filled\n",
			  BLOCK_SIZE(seg),
			  (unsigned long)seg->layout->num_blocks,
			  (unsigned long)count, (unsigned long)filled);
		seg = seg->next;
	}
}

/* for debug */
void
sml_heap_dump()
{
	unsigned int i;
	struct subheap *subheap;
	struct alloc_ptr *ptr;
	struct segment *seg;

	for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
		subheap = &global_subheaps[i];
		ptr = &ALLOC_PTR_SET()->alloc_ptr[i];

		if (BITPTR_EQUAL(ptr->freebit, dummy_bitptr)) {
			sml_debug("ptr[%u] (%u): dummy bitptr\n",
				  i, ptr->blocksize_bytes);
		} else {
			seg = ALLOC_PTR_TO_SEGMENT(ptr);
			sml_debug("ptr[%u] (%u): free=%p, bit %u\n",
				  i, ptr->blocksize_bytes, ptr->free,
				  BITPTR_INDEX(ptr->freebit,
					       BITMAP0_BASE(seg)));
		}
		sml_debug(" segments:\n");
		dump_segment_list(subheap->seglist, *subheap->unreserved);
	}

	sml_debug("freelist:\n");
	dump_segment_list(heap_space.freelist, NULL);
}

static void
set_alloc_ptr(struct alloc_ptr *ptr, struct segment *seg)
{
	if (seg) {
		ptr->free = BLOCK_BASE(seg);
		BITPTR_INIT(ptr->freebit, BITMAP0_BASE(seg), 0);
	} else {
		ptr->free = NULL;
		ptr->freebit = dummy_bitptr;
	}
}

static void
clear_bitmap(struct segment *seg)
{
	unsigned int i;
	memset(BITMAP0_BASE(seg), 0, seg->layout->bitmap_size);

	for (i = 0; i < SEG_RANK; i++)
		BITMAP_LIMIT(seg, i)[-1] = BITMAP_SENTINEL(seg, i);
	seg->live_count = 0;
}

static void
clear_all_bitmaps()
{
	unsigned int i;
	struct segment *seg;

	for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
		for (seg = global_subheaps[i].seglist; seg; seg = seg->next)
			clear_bitmap(seg);
	}
}

static void
init_segment(struct segment *seg, unsigned int blocksize_log2)
{
	const struct segment_layout *layout;
	unsigned int i;
	void *old_limit, *new_limit;

	ASSERT(BLOCKSIZE_MIN_LOG2 <= blocksize_log2
	       && blocksize_log2 <= BLOCKSIZE_MAX_LOG2);

	/* if seg is already initialized, do nothing. */
	if (seg->blocksize_log2 == blocksize_log2) {
		ASSERT(check_segment_consistent(seg, 0) == 0);
		return;
	}

	layout = &segment_layout[blocksize_log2];

	/*
	 * bitmap and stack area are cleared except bitmap sentinels.
	 * Under this assumption, we initialize bitmap and stack area
	 * with accessing least memory.
	 */
	old_limit = ADD_OFFSET(seg, seg->layout->stack_limit);
	new_limit = ADD_OFFSET(seg, layout->stack_limit);
	if ((char*)new_limit > (char*)old_limit)
		memset(old_limit, 0, (char*)new_limit - (char*)old_limit);

	/* clear old sentinel */
	for (i = 0; i < SEG_RANK; i++)
		BITMAP_LIMIT(seg, i)[-1] = 0;
	/* set new sentinel */
	for (i = 0; i < SEG_RANK; i++)
		BITMAP_LIMIT_3(seg,layout,i)[-1] = layout->bitmap_sentinel[i];

	seg->blocksize_log2 = blocksize_log2;
	seg->layout = layout;
	seg->stack = ADD_OFFSET(seg, layout->stack_offset);
	seg->block_base = ADD_OFFSET(seg, layout->block_offset);
	seg->next = NULL;

	ASSERT(check_segment_consistent(seg, 0) == 0);
}

#define GetPageSize()  getpagesize()
#define ReservePageError  ((void*)-1)
#define ReservePage(addr, size) \
	mmap(addr, size, PROT_NONE, MAP_ANON | MAP_PRIVATE, -1, 0)
#define ReleasePage(addr, size) \
	munmap(addr, size)
#define CommitPage(addr, size) \
	mprotect(addr, size, PROT_READ | PROT_WRITE)
#define UncommitPage(addr, size) \
	mmap(addr, size, PROT_NONE, MAP_ANON | MAP_PRIVATE | MAP_FIXED, -1, 0)

#define HEAP_BEGIN_ADDR  NULL

static void
extend_heap(unsigned int count)
{
	unsigned int i;
	struct segment *first, *seg, **seg_p;
	bitptr_t b;

	BITPTR_INIT(b, heap_space.bitmap, 0);
	seg = heap_space.begin;
	count = heap_space.min_num_segments;
	seg_p = &first;
	for (i = 0; count > 0 && i < heap_space.max_num_segments; i++) {
		if (!BITPTR_TEST(b)) {
			CommitPage(seg, SEGMENT_SIZE);
			seg->layout = &segment_layout[0];
			*seg_p = seg;
			seg_p = &seg->next;
			BITPTR_SET(b);
			count--;
			heap_space.num_committed++;
			DBG(("extend: %p (%d) %d", seg, i,
			     heap_space.num_committed));
		}
		BITPTR_INC(b);
		seg = (struct segment *)((char*)seg + SEGMENT_SIZE);
	}
	*seg_p = heap_space.freelist;
	heap_space.freelist = first;
}

static void
init_heap_space(size_t min_size, size_t max_size)
{
	size_t pagesize, alloc_size, reserve_size, freesize_pre, freesize_post;
	unsigned int min_num_segments, max_num_segments, bitmap_bits;
	void *p;

	pagesize = GetPageSize();

	if (SEGMENT_SIZE % pagesize != 0)
		sml_fatal(0, "SEGMENT_SIZE is not aligned in page size.");

	alloc_size = ALIGNSIZE(min_size, SEGMENT_SIZE);
	reserve_size = ALIGNSIZE(max_size, SEGMENT_SIZE);

	if (alloc_size < SEGMENT_SIZE)
		alloc_size = SEGMENT_SIZE;
	if (reserve_size < alloc_size)
		reserve_size = alloc_size;

	min_num_segments = alloc_size / SEGMENT_SIZE;
	max_num_segments = reserve_size / SEGMENT_SIZE;

	p = ReservePage(HEAP_BEGIN_ADDR, SEGMENT_SIZE + reserve_size);
	if (p == ReservePageError)
		sml_fatal(0, "failed to alloc virtual memory.");

	freesize_post = (uintptr_t)p & (SEGMENT_SIZE - 1);
	if (freesize_post == 0) {
		ReleasePage(p + reserve_size, SEGMENT_SIZE);
	} else {
		freesize_pre = SEGMENT_SIZE - freesize_post;
		ReleasePage(p, freesize_pre);
		p = (char*)p + freesize_pre;
		ReleasePage(p + reserve_size, freesize_post);
	}

	heap_space.begin = p;
	heap_space.end = (char*)p + reserve_size;
	heap_space.min_num_segments = min_num_segments;
	heap_space.max_num_segments = max_num_segments;
	heap_space.num_committed = 0;
	heap_space.extend_step = min_num_segments > 0 ? min_num_segments : 1;

	bitmap_bits = ALIGNSIZE(max_num_segments, BITPTR_WORDBITS);
	heap_space.bitmap = xmalloc(bitmap_bits / CHAR_BIT);
	memset(heap_space.bitmap, 0, bitmap_bits / CHAR_BIT);

	extend_heap(min_num_segments);

}

static void
init_subheaps()
{
	unsigned int i;
	struct subheap *subheap;

	for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
		subheap = &global_subheaps[i];
		subheap->seglist = NULL;
		subheap->unreserved = &subheap->seglist;
	}
}

static struct segment *
new_segment()
{
	struct segment *seg;

	FREELIST_NEXT(heap_space.freelist, seg);
	if (seg == NULL)
		return NULL;
	seg->next = NULL;
	return seg;
}

static void
free_segment(struct segment *seg)
{
	bitptr_t b;
	unsigned int index;

	index = ((char*)seg - (char*)heap_space.begin) / SEGMENT_SIZE;
	BITPTR_INIT(b, heap_space.bitmap, index);
	ASSERT(BITPTR_TEST(b));
	BITPTR_CLEAR(b);
	UncommitPage(seg, SEGMENT_SIZE);
	heap_space.num_committed--;
	DBG(("free_segment: %p (%d) %d\n", seg, index,
	     heap_space.num_committed));
}

static void
shrink_heap(unsigned int count)
{
	struct segment *seg;

	while (heap_space.num_committed > heap_space.min_num_segments
	       && count > 0) {
		seg = heap_space.freelist;
		if (seg == NULL)
			break;
		heap_space.freelist = seg->next;
		free_segment(seg);
		heap_space.num_committed--;
		count--;
	}
}

static void
init_alloc_ptr_set(union alloc_ptr_set *ptr_set)
{
	unsigned int i;

	for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
		ptr_set->alloc_ptr[i].blocksize_bytes = 1U << i;
		set_alloc_ptr(&ptr_set->alloc_ptr[i], NULL);
	}
}

void
sml_heap_init(size_t min_size, size_t max_size)
{
	init_heap_space(min_size, max_size);
	init_subheaps();
}

void
sml_heap_free()
{
	ReleasePage(heap_space.begin,
		    (char*)heap_space.end - (char*)heap_space.begin);
}

static unsigned int stack_last;
static void *stack_top = &stack_last;

void stacklist(){
	void *obj = stack_top;
	while (obj != &stack_last) {
		sml_notice("%p", obj);
		obj = obj_to_stack(obj)->next_obj;
	}
}

#define GCSTAT_PUSH_COUNT()

#define OBJ_HAS_NO_POINTER(obj)			\
	(!(OBJ_TYPE(obj) & OBJTYPE_BOXED)	\
	 || (OBJ_TYPE(obj) == OBJTYPE_RECORD	\
	     && OBJ_BITMAP(obj)[0] == 0		\
	     && OBJ_NUM_BITMAPS(obj) == 1))

#define MARKBIT(b, index, seg) do {					\
	unsigned int index__ = (index);					\
	ASSERT(BITPTR_INDEX((b), BITMAP0_BASE(seg)) == index__);	\
	(seg)->live_count++;						\
	BITPTR_SET(b);							\
	if (~BITPTR_WORD(b) == 0U) {					\
		unsigned int i__;					\
		for(i__ = 1; i__ < SEG_RANK; i__++) {			\
			bitptr_t b__;					\
			index__ /= BITPTR_WORDBITS;			\
			BITPTR_INIT(b__, BITMAP_BASE(seg, i__),		\
				    index__);				\
			BITPTR_SET(b__);				\
			if (~BITPTR_WORD(b__) != 0U)			\
				break;					\
		}							\
	}								\
} while (0)

#define STACK_TOP() (stack_top == &stack_last ? NULL : stack_top)
#define STACK_PUSH(obj, seg, index) do {				\
	GCSTAT_PUSH_COUNT();						\
	ASSERT(OBJ_TO_SEGMENT(obj) == seg);				\
	ASSERT(OBJ_TO_INDEX(OBJ_TO_SEGMENT(obj), obj) == index);	\
	ASSERT((seg)->stack[index].next_obj == NULL);			\
	(seg)->stack[index].next_obj = stack_top, stack_top = (obj);	\
} while (0)
#define STACK_POP(topobj) do {						\
	struct segment *seg__ = OBJ_TO_SEGMENT(topobj);			\
	unsigned int index__ = OBJ_TO_INDEX(seg__, topobj);		\
	stack_top = seg__->stack[index__].next_obj;			\
	seg__->stack[index__].next_obj = NULL;				\
} while (0)

SML_PRIMITIVE void
sml_write(void *objaddr, void **writeaddr, void *new_value)
{
	*writeaddr = new_value;
	if (!IS_IN_HEAP(writeaddr))
		sml_global_barrier(writeaddr, objaddr);
}

static void
push(void **block)
{
	void *obj = *block;
	struct segment *seg;
	size_t index;
	bitptr_t b;

	if (!IS_IN_HEAP(obj)) {
		DBG(("%p at %p outside", obj, block));
		if (obj != NULL)
			sml_trace_ptr(obj);
		return;
	}

	seg = OBJ_TO_SEGMENT(obj);
	index = OBJ_TO_INDEX(seg, obj);
	BITPTR_INIT(b, BITMAP0_BASE(seg), index);
	if (BITPTR_TEST(b)) {
		DBG(("already marked: %p", obj));
		return;
	}
	MARKBIT(b, index, seg);
	DBG(("MARK: %p", obj));

	if (OBJ_HAS_NO_POINTER(obj)) {
		DBG(("EARLYMARK: %p", obj));
		return;
	}

	STACK_PUSH(obj, seg, index);
	DBG(("PUSH: %p", obj));
}

static void
mark(void **block)
{
	void *obj = *block;
	struct segment *seg;
	size_t index;
	bitptr_t b;

	ASSERT(STACK_TOP() == NULL);

	if (!IS_IN_HEAP(obj)) {
		DBG(("%p at %p outside", obj, block));
		if (obj != NULL)
			sml_trace_ptr(obj);
		return;
	}

	seg = OBJ_TO_SEGMENT(obj);
	index = OBJ_TO_INDEX(seg, obj);
	BITPTR_INIT(b, BITMAP0_BASE(seg), index);
	if (BITPTR_TEST(b)) {
		DBG(("already marked: %p", obj));
		return;
	}
	MARKBIT(b, index, seg);
	DBG(("MARK: %p", obj));

	if (OBJ_HAS_NO_POINTER(obj)) {
		DBG(("EARLYMARK: %p", obj));
		return;
	}

	for (;;) {
		sml_obj_enum_ptr(obj, push);
		obj = STACK_TOP();
		if (obj == NULL) {
			DBG(("MARK END"));
			break;
		}
		STACK_POP(obj);
		DBG(("POP: %p", obj));
	}
}

static void
sweep()
{
	unsigned int i;
	struct subheap *subheap;
	struct segment *seg;
	struct segment **filled_tail;
	struct segment *unfilled, **unfilled_tail;
	struct segment *free, **free_tail;

	/*
	 * The order of segments in a sub-heap and the free list may be
	 * significant for performace.
	 *
	 * Segments in the list should be ordered in allocation time order.
	 * By keeping this order, mutator always tries to find a free block
	 * at first from long-alived segments, which have long-life objects.
	 * This storategy is good to gather long-life objects in one segment
	 * as many as possible.
	 *
	 * Segments in the free list should be sorted by previous block size.
	 * Smaller block size segment has larger bitmap, and any bitmap in
	 * the free list are already cleared by collector.
	 * By recycling smaller block size segment at first, we can avoid
	 * memset of segment initialization as many as possible.
	 */
	free = NULL, free_tail = &free;
	for (i = BLOCKSIZE_MIN_LOG2; i <= BLOCKSIZE_MAX_LOG2; i++) {
		subheap = &global_subheaps[i];

		filled_tail = &subheap->seglist;
		unfilled = NULL, unfilled_tail = &unfilled;
		for (seg = subheap->seglist; seg; seg = seg->next) {
			if (seg->live_count == seg->layout->num_blocks) {
				*filled_tail = seg;
				filled_tail = &seg->next;
			} else if (seg->live_count > 0) {
				*unfilled_tail = seg;
				unfilled_tail = &seg->next;
			} else {
				*free_tail = seg;
				free_tail = &seg->next;
			}
		}

		*filled_tail = unfilled;
		*unfilled_tail = NULL;
		subheap->unreserved = filled_tail;
	}
	*free_tail = heap_space.freelist;
	heap_space.freelist = free;
}

static void
do_gc()
{
	DBG(("start gc"));

		clear_all_bitmaps();

	sml_rootset_enum_ptr(mark);
	sml_malloc_pop_and_mark(mark);

	/* check finalization */
	sml_check_finalizer(mark);

		sweep();
		shrink_heap(1);

	/* sweep malloc heap */
	sml_malloc_sweep();


	DBG(("gc finished."));
}

void
sml_heap_gc()
{
	do_gc();
	sml_run_finalizer(NULL);
}

#define GCSTAT_ALLOC_COUNT(counter, offset, size)
#define GCSTAT_TRIGGER(slogsize_log2)
#define GCSTAT_COUNT_MOVE(counter1, counter2)

static NOINLINE void *
find_bitmap(struct alloc_ptr *ptr)
{
	unsigned int i, index, *base, *limit, *p;
	struct segment *seg;
	bitptr_t b = ptr->freebit;
	void *obj;

	ASSERT(ptr->freebit.ptr != &dummy_bitmap);
	seg = ALLOC_PTR_TO_SEGMENT(ptr);

	BITPTR_NEXT(b);
	base = BITMAP0_BASE(seg);

	if (BITPTR_NEXT_FAILED(b)) {
		for (i = 1;; i++) {
			if (i >= SEG_RANK) {
				p = &BITPTR_WORD(b) + 1;
				limit = BITMAP_LIMIT(seg, SEG_RANK - 1);
				b = bitptr_linear_search(p, limit);
				if (BITPTR_NEXT_FAILED(b))
					return NULL;
				i = SEG_RANK - 1;
				break;
			}
			index = BITPTR_WORDINDEX(b, base) + 1;
			base = BITMAP_BASE(seg, i);
			BITPTR_INIT(b, base, index);
			BITPTR_NEXT(b);
			if (!BITPTR_NEXT_FAILED(b))
				break;
		}
		do {
			index = BITPTR_INDEX(b, base);
			base = BITMAP_BASE(seg, --i);
			BITPTR_INIT(b, base + index, 0);
			BITPTR_NEXT(b);
			ASSERT(!BITPTR_NEXT_FAILED(b));
		} while (i > 0);
	}

	index = BITPTR_INDEX(b, base);
	obj = BLOCK_BASE(seg) + (index << seg->blocksize_log2);
	ASSERT(OBJ_TO_SEGMENT(obj) == seg);

	GCSTAT_ALLOC_COUNT(find, seg->blocksize_log2, ptr->blocksize_bytes);
	BITPTR_INC(b);
	ptr->freebit = b;
	ptr->free = (char*)obj + ptr->blocksize_bytes;

	return obj;
}

static NOINLINE void *
find_segment(struct alloc_ptr *ptr)
{
	unsigned int blocksize_log2 = ALLOC_PTR_TO_BLOCKSIZE_LOG2(ptr);
	struct segment *seg;
	struct subheap *subheap = &global_subheaps[blocksize_log2];
	void *obj;

	ASSERT(BLOCKSIZE_MIN_LOG2 <= blocksize_log2
	       && blocksize_log2 <= BLOCKSIZE_MAX_LOG2);

	UNRESERVED_NEXT(subheap->unreserved, seg);
	if (seg) {
		/* seg have at least one free block. */
		set_alloc_ptr(ptr, seg);
		obj = find_bitmap(ptr);
		ASSERT(obj != NULL);
		GCSTAT_COUNT_MOVE(find[blocksize_log2], next[blocksize_log2]);
		return obj;
	}

	seg = new_segment();
	if (seg) {
		init_segment(seg, blocksize_log2);
		UNRESERVED_APPEND(subheap->unreserved, seg);
		set_alloc_ptr(ptr, seg);

		ASSERT(!BITPTR_TEST(ptr->freebit));
		GCSTAT_ALLOC_COUNT(new, blocksize_log2, ptr->blocksize_bytes);
		BITPTR_INC(ptr->freebit);
		obj = ptr->free;
		ptr->free += ptr->blocksize_bytes;
		return obj;
	}

	return NULL;
}

SML_PRIMITIVE void *
sml_alloc(unsigned int objsize, void *frame_pointer)
{
	size_t alloc_size;
	unsigned int blocksize_log2;
	struct alloc_ptr *ptr;
	void *obj;


	/* ensure that alloc_size is at least BLOCKSIZE_MIN. */
	alloc_size = ALIGNSIZE(OBJ_HEADER_SIZE + objsize, BLOCKSIZE_MIN);

	if (alloc_size > BLOCKSIZE_MAX) {
		GCSTAT_ALLOC_COUNT(malloc, 0, alloc_size);
		sml_save_frame_pointer(frame_pointer);
		return sml_obj_malloc(alloc_size);
	}

	blocksize_log2 = CEIL_LOG2(alloc_size);
	ASSERT(BLOCKSIZE_MIN_LOG2 <= blocksize_log2
	       && blocksize_log2 <= BLOCKSIZE_MAX_LOG2);

	ptr = &ALLOC_PTR_SET()->alloc_ptr[blocksize_log2];

	if (!BITPTR_TEST(ptr->freebit)) {
		GCSTAT_ALLOC_COUNT(fast, blocksize_log2, alloc_size);
		BITPTR_INC(ptr->freebit);
		obj = ptr->free;
		ptr->free += ptr->blocksize_bytes;
		goto alloced;
	}

	sml_save_frame_pointer(frame_pointer);

	if (ptr->free != NULL) {
		obj = find_bitmap(ptr);
		if (obj) goto alloced;
	}
	obj = find_segment(ptr);
	if (obj) goto alloced;

	GCSTAT_TRIGGER(blocksize_log2);
	do_gc(MAJOR);
	obj = find_segment(ptr);
	if (obj) goto alloced_major;

	extend_heap(heap_space.extend_step);
	obj = find_segment(ptr);
	if (obj) goto alloced_major;

	sml_fatal(0, "heap exceeded: intended to allocate %u bytes.",
		  ptr->blocksize_bytes);

 alloced_major:
	ASSERT(check_newobj(obj));
	/* NOTE: sml_run_finalizer may cause garbage collection. */
	obj = sml_run_finalizer(obj);
	goto finished;
 alloced:
	ASSERT(check_newobj(obj));
 finished:
	OBJ_HEADER(obj) = 0;
	return obj;
}
