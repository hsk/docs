/*
 * objspace.c - common heap management features.
 * @copyright (c) 2010, Tohoku University.
 * @author UENO Katsuhiro
 * @version $Id: value.c,v 1.5 2008/02/05 08:54:35 katsu Exp $
 */

#include <stdlib.h>
#include <stdint.h>
#include "smlsharp.h"
#include "sml_obj.h"
#include "control.h"
#include "objspace.h"
#include "sml_tree.h"

/* tree node allocator for persistent trees. */
static sml_obstack_t *persistent_node_obstack = NULL;

static void *
persistent_node_alloc(size_t size)
{
	return sml_obstack_alloc(&persistent_node_obstack, size);
}

static int
voidp_cmp(void *x, void *y)
{
	uintptr_t m = (uintptr_t)x, n = (uintptr_t)y;
	if (m < n) return -1;
	else if (m > n) return 1;
	else return 0;
}

/* barriered slot */

static sml_tree_t global_barrier =
	SML_TREE_INITIALIZER(voidp_cmp, persistent_node_alloc, NULL);

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

static void
dump_malloc(void *item, void *data ATTR_UNUSED)
{
	sml_notice("%p (flags=%08x, size=%lu)", item,
		   MALLOC_HEAD(item)->flags, (unsigned long)OBJ_SIZE(item));
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
	sml_notice("barriered :");
	sml_tree_each(&global_barrier, dump_barrier, NULL);
	sml_notice("mark stack :");
	for (p = malloc_stack_top; p; p = p->next)
		dump_malloc(MALLOC_BODY(p), NULL);
}

/*

グローバルバリアの開放を行う。

グローバルバリアはsml_writeあるいは、sml_global_barrierでバリアされたもので、
gc時にバリアされている物を検索対象にする。

*/
void
sml_finish(void)
{
	global_barrier.root = NULL;
	sml_obstack_free(&persistent_node_obstack, NULL);
}

/* root set management */

/*
バリア内のアイテムのトラバース
トレースの使用にあわせて、一度、アドレスの変換を行う。
*/
static void
each_barrier(void *item, void *data)
{
	void (**trace)(void **) = data;
	void **addr = item;
	(*trace)(addr);
}

/*
ルートセットのトラバース
(gcで使用)

グローバルバリアと、
コントロール内のフレームとテンポラリをトラバースする

*/
static void
sml_rootset_enum_ptr(void (*trace)(void **))
{
	/* global_barrier includes every addresses in
	 * callback_closures where holds an ML object. */
	sml_tree_each(&global_barrier, each_barrier, &trace);
	sml_control_enum_ptr(trace);
}

/*
malloc_stackのトラーバース(内部を見るけど、そのものは見ない)
(gcで使用)

マークしたオブジェクトはここに追加されている。
グローバル

*/
static void
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

/*
sml_malloc_sweep内で使われる、
gcの1アイテムのスイープ処理で、フラグが0なら開放する。
*/
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

/*

malloc_heapをスイープする。(マークされてなければ削除)
(gcで使用)

*/
static void
sml_malloc_sweep()
{
	ASSERT(malloc_stack_top == NULL);

	sml_tree_reject(&malloc_heap, malloc_heap_sweep);
	malloc_count = 0;
}

static void
sml_trace_ptr(void *obj)
{
	// mallocヒープを検索
	if (sml_tree_find(&malloc_heap, obj)) {
		// 見つかったら、ヘッダを取り出し、
		struct malloc_obj_header *head = MALLOC_HEAD(obj);
		ASSERT(head->flags != 0 || head->next == NULL);

		// マークされてなければ
		if (head->flags == 0) {
			// マークスタックに追加して
			head->next = malloc_stack_top;
			malloc_stack_top = head;
		}
		// フラグを立てる
		head->flags |= MALLOC_FLAG_TRACED;
	}
}

static void
trace(void **slot)
{
	sml_trace_ptr(*slot);
}

void
sml_heap_gc()
{
	// ルート集合、
	// すなわちコントロール内のフレーム、テンポラリ、グローバルバリアのトラバース
	sml_rootset_enum_ptr(trace);
	// rootの中味を見る
	sml_malloc_pop_and_mark(trace);
	sml_malloc_sweep();
}

/* malloc heap */

/*

xmallocでメモリを作成して、malloc_heapに登録し、
malloc_stack_topに加える。

*/
static void *
sml_objspace_malloc(size_t objsize)
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

	printf("added malloc heap %p\n\n", obj);
	// レコードがimmtableタイプの場合は、ライトバリアにふくまれないっぽい。todo
	/* if the new object is an immutable object such as record, ML
	 * object pointer may be stored without write barrier during
	 * initialization of the new object. */
	head->next = malloc_stack_top;
	malloc_stack_top = head;
	head->flags = MALLOC_FLAG_REMEMBER;

	OBJ_HEADER(obj) = 0;
	return obj;
}

// アロックするときは、フレームポインタを保存してから、アロックする。
SML_PRIMITIVE void *
sml_alloc(unsigned int objsize, void *frame_pointer)
{
	sml_save_frame_pointer(frame_pointer);
	return sml_objspace_malloc(objsize);
}

/* global barrier */

/*
sml_global_barrierないで使われ、バリアの処理を行う。
objのヘッダのフラグがゼロなら、MALLOC_FLAG_REMEMBERを立てて、malloc_stack_topに追加する。
*/
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

// 第二引数が、malloc_heap中にあれば、第二引数をmallocバリアし、なければ、
// グローバルライトバリアに 第一引数を追加する。
// つまり、sml_allocされたものは、malloc_barrierに追加される。
// １つ追加されていれば、その中味は検索される。
SML_PRIMITIVE void
sml_global_barrier(void **writeaddr, void *obj)
{

	/* check whether obj is in malloc heap. */
	if (sml_tree_find(&malloc_heap, obj)) {
		printf("added mlloc barrier %p\n", obj);
		malloc_barrier(obj);
	} else {
		printf("added global_barrier %p * %p obj=%p\n", writeaddr, *writeaddr, obj);
		/* There is a reference to an ML object from outside.
		 * remember the writeaddr as a root set. */
		sml_tree_insert(&global_barrier, writeaddr);
	}

}

// バリア付きの書き込み
SML_PRIMITIVE void
sml_write(void *objaddr, void **writeaddr, void *new_value)
{
	*writeaddr = new_value;
	sml_global_barrier(writeaddr, objaddr);
}
