// added bitmap
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

#define TRUE -1
#define FALSE 0

#ifdef MINGW32
#define GetPageSize()  (64 * 1024)
#define ReservePageError  NULL
#define ReservePage(addr, size)	\
	VirtualAlloc(addr, size, MEM_RESERVE, PAGE_NOACCESS)
#define ReleasePage(addr, size) \
	VirtualFree(addr, size, MEM_RELEASE)
#define CommitPage(addr, size) \
	VirtualAlloc(addr, size, MEM_COMMIT, PAGE_EXECUTE_READWRITE)
#define UncommitPage(addr, size) \
	VirtualFree(addr, size, MEM_DECOMMIT)
#else
#include <sys/mman.h>
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
#endif /* MINGW32 */

typedef unsigned int uint;

typedef struct Seg {
  int bitmap[128*1024/8/32];
  int size;
  struct Seg* next;
  char data[128*1024];
} Seg;

typedef struct Heap {
  int size;
  Seg* seg;
  int ptr;
} Heap;

// log2のfloorを計算 2なら0 4なら1 8なら2
// compute floor(log2(x))
uint Math_floor_log2(uint x) {
	uint r = 0;
	while (x >>= 1) r++;
	return r;
}

Seg* Seg_init(int size) {
	printf("seg_init %d\n", size);
	Seg* seg = (Seg*)malloc(sizeof(Seg));
	seg->size = size;
	seg->next = NULL;
	return seg;
}

Seg* Seg_add(Seg* seg) {
	seg->next = Seg_init(seg->size);
	return seg->next;
}

void* Seg_alloc(Seg* seg, Heap* heap) {
	int size = seg->size;
	int n;
	int ptr;
	do {
		n = heap->ptr * size;
		printf("seg_alloc %d %d\n", size, heap->ptr);
		printf("n=%d\n", n);
		while (n > 128*1024) {
			if(seg->next == NULL) {
				seg = Seg_add(seg);
				if(seg == NULL) {
					printf("seg get error size=%d\n", size);
					exit(0);
				}
				break;
			} else {
				seg = seg->next;
			}
			n -= 128*1024;
		}
		ptr = n / size;
		printf("n=%d\n", ptr);
		heap->ptr++;
		if ((seg->bitmap[ptr/32] & (1 << (ptr % 32))) != 0){
			printf("continue");
			continue;
		}
	} while (FALSE);
	seg->bitmap[ptr/32] |= 1 << (ptr % 32);
	return &(seg->data[n]);
}

Heap* Heap_init(int size) {
	printf("heap_init %d\n", size);
	Heap *heap = (Heap*)malloc(sizeof(Heap));
	heap->size = size;
	Seg *seg = Seg_init(size);
	heap->seg = seg;
	heap->ptr = 0;
	return heap;
}

void* Heap_alloc(Heap* heap) {
	void* data = Seg_alloc(heap->seg, heap);
	return data;
}

Heap *heaps[12];

void Gc_init() {
	for(int i = 2; i <= 11; i++)
		heaps[i] = Heap_init(2 << i);
}

void Gc_collect() {
	// mark

	// clear bitmaps
	for(int i = 2; i <= 11; i++) {

		Seg* seg = heaps[i]->seg;
		while(seg) {
			memset(seg->bitmap, 0, 128*1024/seg->size/32);
			seg = seg->next;
		}
		heaps[i]->ptr = 0;
	}

	// mark all

	// sweep
}

void* Gc_alloc(int size) {
	int h = Math_floor_log2(size);
	if (h < 2) h = 2;
	else if (h > 11) {
		printf("gc alloc size error %d\n", size);
		exit(0);
	}
	printf("size=%d h=%d\n",size, h);
	Heap* heap = heaps[h];
	void* data = Heap_alloc(heap);
	return data;
}

#include "object.h"

void* Obj_newAllay(int size) {
	int *data = (int*)Gc_alloc(size+sizeof(int));
	data[0]= OBJ_UNBOXED_ARRAY;
	return (void*)&data[1];
}

void* Obj_newVAllay(int size) {
	int *data = (int*)Gc_alloc(size+sizeof(int));
	data[0]= OBJ_BOXED_ARRAY;
	return (void*)&data[1];
}

void
Obj_each(void* obj, void (*trace)(void **)) {
	int tag = ((int*)obj)[-1];
	switch(tag){
		case OBJ_UNBOXED_ARRAY:
			break;
		case OBJ_BOXED_ARRAY:

	}
}

int main() {
	Gc_init();
	for(int i = 0; i < 4097; i++) {
		void *data = Gc_alloc(i);
		printf("%p\n", data);
	}
	return 0;
}
