#include <stdio.h>
#include <sys/time.h>
#include <memory.h>

long gett() {
  timeval tv;
  gettimeofday((&tv), NULL);
  return (((tv . tv_sec) * 1000) + ((tv . tv_usec) / 1000));
} 

struct Vec {
  int size;
  void** data;
};

Vec* newVec() {
  Vec* v = new Vec();
  v->size = 0;
  v->data = new void*[v->size];
  return v;
}
void setVec(Vec* v, int idx, void* d) {
  if (idx >= v->size) {
    // resize
    void** data = new void*[idx+1];
    memcpy(data, v->data, sizeof(void*)*v->size);
    v->size = idx+1;
    delete[] v->data;
    v->data = data;
  }
  v->data[idx] = d;
}

struct Class {
  int id;
};

int Class_genId() {
  static int classId = -1;
  classId++;
  return classId;
}
