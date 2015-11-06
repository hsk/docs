#include <stdio.h>
typedef struct FrameMap {
  unsigned short frame_size;
  void* start;
  void* end;
  struct FrameMap* next;
} FrameMap;

FrameMap* gc_frame_map_list;


void gc_add_frame_map(FrameMap* frame_map) {
  printf("framemap start %p end %p size=%d\n",
    frame_map->start,frame_map->end,
    frame_map->frame_size);

  frame_map->next = gc_frame_map_list;
  gc_frame_map_list = frame_map;
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

FrameMap* gc_mark_find_frame_map(void* addr) {
  FrameMap* frame_map = gc_frame_map_list;
  while (frame_map) {
    if (frame_map->start <= addr && addr <= frame_map->end)
      return frame_map;      
    frame_map = frame_map->next;
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

    FrameMap* frame_map = gc_mark_find_frame_map(addr);
    if (!frame_map) continue;

#ifdef __x86_64__
    void** objects = (void**)&ptr[-2 - frame_map->frame_size];
#else
    void** objects = (void**)&ptr[-2 - frame_map->frame_size];
#endif

    printf("%p %d\n", objects, frame_map->frame_size);

    for(int i = 0; i < frame_map->frame_size; i++) {
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
  static FrameMap f = {10, (void*)f2,&&end, NULL};
  gc_add_frame_map(&f); start_ptr=&&start; goto *start_ptr;
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
  static FrameMap f = {20, (void*)f1,&&end, NULL};
  gc_add_frame_map(&f); start_ptr=&&start; goto *start_ptr;
}

int main() {
  gc_init();
  f1();
  return 0;
}

