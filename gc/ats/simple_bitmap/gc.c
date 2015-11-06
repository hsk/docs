#include <stdio.h>
#include <stdlib.h>

typedef unsigned int uint;

typedef struct Seg {
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

void* Seg_get(Seg* seg, int ptr) {
	int size = seg->size;
	printf("seg_get %d %d\n", size, ptr);
	int n = ptr * size;
	printf("n=%d\n", n);
	while (n > 128*1024) {
		printf("a\n");
		if(seg->next == NULL) {
			seg = Seg_add(seg);
			if(seg == NULL) {
				printf("seg get error size=%d\n", size);
				exit(0);
			}
		} else {
			seg = seg->next;
		}
		n -= 128*1024;
	}
	printf("n=%d\n", n);
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
	void* data = Seg_get(heap->seg, heap->ptr);
	heap->ptr++;
	return data;
}

Heap *heaps[12];

void Gc_init() {
	for(int i = 2; i <= 11; i++)
		heaps[i] = Heap_init(2 << i);
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

int main() {
	Gc_init();
	
	for(int i = 0; i < 4097; i++) {
		void *data = Gc_alloc(i);
		printf("%p\n", data);
	}
	return 0;
}
