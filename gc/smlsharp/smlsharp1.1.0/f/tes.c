#include <stdio.h>

typedef struct Array {
        int size;
	void (*each)(void *item1);
} Array;


void each_array(Array *array, dt){
  char *data = (char*)array->root;

  for(int i=0,pos=0;i<array->length;i++,pos+=array->size){
    array->each((void*)&data[pos]);
  }
}

void each_int(void* data) {
  int i = *((int*)data);
  printf("%d\n", i);
}
Array a = {sizeof(int), each_int};
int main() {
  int dt[] = {0,1,2};
  each_array(&a,dt,3);

  return 0;
}
