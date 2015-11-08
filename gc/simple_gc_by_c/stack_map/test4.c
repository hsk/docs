#include <stdio.h>
typedef struct StackMap {
  unsigned short frame_size;
  void* start;
  void* end;
  struct StackMap* next;
} StackMap;

StackMap* gc_stack_map_list;


void gc_add_stack_map(StackMap* stack_map) {
  printf("stack map start %p end %p size=%d\n",
    stack_map->start, stack_map->end,
    stack_map->frame_size);

  stack_map->next = gc_stack_map_list;
  gc_stack_map_list = stack_map;
}

void** gc_top_ptr;

void** get_stack_top() {
  void* data;
  void** ptr = &data;
  ptr = (void**)&(ptr[1]);
  return (void**)ptr[0];
}

void gc_init() {
  gc_top_ptr = get_stack_top();
  printf("top = %p\n", gc_top_ptr);
}

StackMap* gc_mark_find_stack_map(void* addr) {
  StackMap* stack_map = gc_stack_map_list;
  while (stack_map) {
    if (stack_map->start <= addr && addr <= stack_map->end)
      return stack_map;      
    stack_map = stack_map->next;
  }
  return NULL;
}


void gc_mark(void**ptr, void* addr) {
/*
  for(int i = -4; i < 50; i++) {
    printf("%p %p\n", &ptr[i], ptr[i]);
  }*/

  do {
    addr = ptr[1];
    ptr = (void**)(ptr[0]);

    printf("ptr=%p %p\n", ptr, ptr[1]);

    StackMap* stack_map = gc_mark_find_stack_map(addr);
    if (!stack_map) continue;

#ifdef __x86_64__
    void** objects = (void**)&ptr[-2 - stack_map->frame_size];
#else
    void** objects = (void**)&ptr[-2 - stack_map->frame_size];
#endif

    printf("%p %d\n", objects, stack_map->frame_size);

    for(int i = 0; i < stack_map->frame_size; i++) {
      printf("  %d %p\n", i, objects[i]);
    }

  } while(ptr <= gc_top_ptr);

}

void gc_collect() {
  void* data;
  printf("gc %p\n", &data+1);// スタックトップ

  gc_mark(get_stack_top(),NULL);
}

long long f2() {
  long data[10];
  void** frame = data;
  static void* start_ptr = &&end; goto *start_ptr; start:;
  void* data2;

  frame[0]=(void*)1;
  frame[1]=(void*)2;
  printf("f2 %p\n", &data+2);// スタックトップ

  gc_collect();
  return 1;
end:;
  static StackMap f = {10, (void*)f2,&&end, NULL};
  gc_add_stack_map(&f); start_ptr=&&start; goto *start_ptr;
}

int f1() {
  long data[20];
  void** frame = data;
  static void* start_ptr = &&end; goto *start_ptr; start:;

  frame[0]=(void*)1;
  frame[1]=(void*)2;
  frame[2]=(void*)3;
  frame[3]=(void*)4;
  frame[4]=(void*)5;
  frame[5]=(void*)6;
  frame[6]=(void*)7;
  frame[7]=(void*)8;
  frame[8]=(void*)9;
  frame[9]=(void*)10;

  f2();
  return 0;
end:;
  static StackMap f = {20, (void*)f1,&&end, NULL};
  gc_add_stack_map(&f); start_ptr=&&start; goto *start_ptr;
}

int main() {
  gc_init();
  f1();
  return 0;
}

