/*

とても簡単な、完全なGCをC言語で使うプログラム

*/
#include <stdio.h>
#include <stdlib.h>
#define true
#define false
#define bool int

typedef struct Heap {
  int header;
  struct Heap* next;
  int data;
} Heap;

// ヒープからアロケーションする
Heap* heap_alloc(Heap **heap_top, int size) {
  Heap* heap = malloc(sizeof(heap)-sizeof(int*)+size);// サイズ分マロック
  // ヒープに登録
  heap->next = *heap_top;
  *heap_top = heap;
  return heap;
}

// ヒープに加える
void heap_add(Heap **heap_top, void* data) {
  Heap* heap = &data[sizeof(int)-sizeof(Heap)];
  // ヒープに登録
  heap->next = *heap_top;
  *heap_top = heap;
}

// ヒープからはずす
bool heap_delete(Heap** heap_top, void* data) {
  Heap* top = *heap_top;
  while(top) {
    if(&top->next->data==data) {// みつかった。
       top->next=heap->next;
       return true;
    }
    top = top->next;
  }
  return false;
}

bool heap_free(Heap** heap_top, void* data) {
  Heap* top = *heap_top;
  bool ret = heap_delete(heap_top, data);
  free(top);
  return ret;
}

// ヒープを検索する
Heap* heap_find(Heap **heap_top, void* data) {
  Heap* top = *heap_top;
  while(top) {
    if(&top->data==data) return top;
    top = top->next;
  }
  return NULL;
}

Heap* gc_heap;
Heap* root_heap;

// ルート集合にアドレスを登録する
void root_add(void* data) {
  heap_add(&root_heap, data);
}

// ルートから削除する
void root_delete(void* data) {
  heap_delete(&root_heap, data);
}

// gcヒープからアロケーションする
void* gc_alloc(int size) {
  return heap_alloc(&gc_heap, size);
}

// gcヒープから開放する
bool gc_free(void* data) {
  return heap_free(&gc_heap, data);
}

Heap* mark_heap;

// マーク ルート集合をマークして、マークヒープに加える。
void gc_mark() {
  Heap* top = *root_heap;
  while(top) {
    if(top->next->data==data) {// みつかった。
       Heap* heap = top->next;
       free(heap);
       top->next=heap->next;
       return;
    }
    top = top->next;
  }
}

void gc_sweep() {

}

// gcを実行する
void gc_collect() {
  gc_mark();
  gc_sweep();
}

int main() {
  printf("test\n");
}
