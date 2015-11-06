import core.stdc.stdio;


void f0() {synchronized {
  void** ptr;
  void* data[1];
  data[0]=cast(void*)0xf0f0f0f0;
  printf("f0 %p\n", &data);
  ptr = cast(void**)&data;

  for(int i = -4; i < 50; i++) {
    printf("%p %p\n", &ptr[i], ptr[i]);
  }
  printf("-----\n");

  for(int i = 0; i < 4; i++) {
    printf("%p %p\n",
      &ptr[1],
      ptr[1]);
    ptr = cast(void**)(ptr[0]);
  }
  printf("return\n");

}}

void f1() {
  void* data[2];// 2個以上の奇数を取れば多分トップに来る x86_64だと3個以上必要そう。
synchronized {
  void* data2[20];
  data2[0]=cast(void*)0xf1f10B01;
  data2[1]=cast(void*)0xf1f10B02;
  data2[2]=cast(void*)0xf1f10B03;
  data2[3]=cast(void*)0xf1f10B04;
  data2[4]=cast(void*)0xf1f10B04;
  data2[5]=cast(void*)0xf1f10B04;

  data[0]=cast(void*)0xf1f1f0;
  data[1]=cast(void*)0xf1f1f1;
  void* dummy1=cast(void*)0xf10001;
  void* dummy2=cast(void*)0xf20002;
  void* dummy3=cast(void*)0xf30003;
  printf("f1 %p\n", &data[0]);
}
  f0();
//  int data2 = 0xf1aaaaf1;
}

void f2() { 
  void* data[2];
synchronized{
  data[0]=cast(void*)0xf2f2f1;
  data[1]=cast(void*)0xf2f2f2;
  printf("f2 %p\n", &data[0]);
}  f1();
}

void f3() { 
  void* data[6];
synchronized{
  data[0]=cast(void*)0xf3f3f1;
  data[1]=cast(void*)0xf3f3f2;
  data[2]=cast(void*)0xf3f3f3;
  data[3]=cast(void*)0xf3f3f4;
  data[4]=cast(void*)0xf3f3f5;
  data[5]=cast(void*)0xf5f5f6;
  printf("f3 %p\n", &data[0]);
}  f2();
}

int main() {
  f3();
  return 0;
}


