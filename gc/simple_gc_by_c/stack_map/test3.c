#include <stdio.h>

void** get_stack_top() {
  void* data;
  void** ptr = &data;
  ptr = (void**)&(ptr[1]);
  return (void**)ptr[0];
}

void f0() {
  volatile long data=0xf0f0f0f0;
  printf("f0 %p %p\n", &data, get_stack_top());
  void** ptr = get_stack_top();
  ptr = (void**)(ptr[0]);
  for(int i = -4; i < 50; i++) {
    printf("%p %p\n", &ptr[i], ptr[i]);
  }
  printf("-----\n");

  for(int i = 0; i < 4; i++) {
    printf("%p %p\n",
#ifdef __x86_64__
      &ptr[-3],
      ptr[-3]);
#else
      &ptr[-2],
      ptr[-2]);
#endif
    ptr = (void**)(ptr[0]);
  }
  printf("return\n");
}

void f1() {
  volatile long data1;// 2個以上の奇数を取れば多分トップに来る x86_64だと3個以上必要そう。
  volatile long data;// 2個以上の奇数を取れば多分トップに来る x86_64だと3個以上必要そう。
  {
  long data2[6];
  data2[0]=0xf1f10B01;
  data2[1]=0xf1f10B02;
  data2[2]=0xf1f10B03;
  data2[3]=0xf1f10B04;
  data2[4]=0xf1f10B05;
  data2[5]=0xf1f10B06;
  data1=0xfff1f0;
  data=0xfff1f1;
  register long dummy1=0xf10001;
  register long dummy2=0xf20002;
  register long dummy3=0xf30003;

  f0();
  }
//  int data2 = 0xf1aaaaf1;
}

void f2() {
  volatile long data[2];
  data[0]=0xf2f2f1;
  data[1]=0xf2f2f2;
  printf("f2 %p %p\n", &data[0], get_stack_top());
  f1();
}

void f3() {
  volatile long data[6];
  data[0]=0xf3f3f1;
  data[1]=0xf3f3f2;
  data[2]=0xf3f3f3;
  data[3]=0xf3f3f4;
  data[4]=0xf3f3f5;
  data[5]=0xf5f5f6;
  printf("f3 %p %p\n", &data[0], get_stack_top());
  f2();
}

int main() {
  f3();
}

