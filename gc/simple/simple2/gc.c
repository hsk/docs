/*

C だけで使える簡単な完全なGCをするためのサンプルプログラム

*/
#include "gc.h"

#ifdef DEBUG
#else
void noprintf(char* str, ...){}
#endif

Frame* frame_list;
ObjectHeader* heap_list;
int heap_num;
int heap_max;

int heap_find(ObjectHeader* o) {
  ObjectHeader* object = heap_list;
  while (object) {
    if(object == o) return 1;
    object = object->next;
  }
  return 0;
}

void gc_mark_object(Object* object) {
  ObjectHeader* head = &((ObjectHeader*)object)[-1];
  debug("mark %p\n",head);
  long size;
  if (!heap_find(head)) return;
  if (head->marked) return;
  long* bitmap;
  head->marked = 1;
  switch(head->type) {
    case OBJ_BOXED_ARRAY:
      size = ((int)head->size) / sizeof(long);
      debug("size=%ld\n",size);
      for(int i = 0; i < size; i++)
          gc_mark_object(object->field[i]);
      break;
    case OBJ_PAIR:
      debug("PAIR\n");
      gc_mark_object(object->pair.fst);
      gc_mark_object(object->pair.snd);
      break;
    case OBJ_UNBOXED_ARRAY:
      break;
    case OBJ_RECORD:
      size = ((int)head->size) / sizeof(long);
      debug("RECORD size=%ld\n", size);
      bitmap = &object->longs[size];
      debug("size=%ld\n",size);
      for(int i = 0; i < size; i++) {
        if(bitmap[i/sizeof(long)] & (1 << (i % sizeof(long))))
          gc_mark_object(object->field[i]);
        else
          debug("skip %d\n", i);
      }
      break;
  }
}

void gc_mark() {
  Frame* frame = frame_list;
  while(frame) {
    for(int i = 0; i < frame->frame_size; i++)
      gc_mark_object(frame->frame_data[i]);
    frame = frame->frame_prev;
  }
}

void gc_sweep() {
  ObjectHeader** object = &heap_list;
  while (*object) {
    if (!(*object)->marked) {
      ObjectHeader* unreached = *object;
      *object = unreached->next;
      free(unreached);

      heap_num--;
    } else {
      (*object)->marked = 0;
      object = &(*object)->next;
    }
  }
}

void gc_collect() {
  int prev_num = heap_num;

  gc_mark();
  gc_sweep();

  heap_max = prev_num * 2;

  debug("Collected %d objects, %d remaining.\n", prev_num - heap_num,
         heap_num);
}

void* gc_alloc(ObjectType type, int size) {
  if (heap_num == heap_max) gc_collect();

  ObjectHeader* head = malloc(sizeof(ObjectHeader)+size);

  debug("gc_alloc %p\n", head);
  head->type = type;
  head->next = heap_list;
  heap_list = head;
  head->marked = 0;
  head->size=size;
  heap_num++;

  return &head[1];
}

void* gc_alloc_int(int n) {
  int* data = gc_alloc(OBJ_UNBOXED_ARRAY, sizeof(int)*1);

  debug("int ptr %p\n", data);
  *data = n;
  return data;
}

void gc_init() {
  frame_list = NULL;
  heap_list = NULL;
  heap_num = 0;
  heap_max = 8;
}

void gc_free() {
  frame_list = NULL;
  gc_collect();
}
