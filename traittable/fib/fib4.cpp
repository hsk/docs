#include <stdio.h>
#include <memory.h>
#include <stdio.h>
#include <sys/time.h>

long gett() {
  timeval tv;
  gettimeofday (&tv, NULL);
  return (tv.tv_sec) * 1000 + tv.tv_usec / 1000;
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
	if (idx > v->size) {
		// resize
		void** data = new void*[idx+1];
		memcpy(v->data, data, sizeof(void*)*v->size);
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

// struct Int
struct Int {
	int id;
	int x;
};
int Int_classId = Class_genId();
Int* newInt(int x) {
	Int* point = new Int();
	point->id = Int_classId;
	point->x = x;
	return point;
}

struct Fib2 {
	int (*fib)(Class*);
};
Vec* Fib2_v = newVec();

int Fib2_Int_fib(Class* self) {
	Int* p = (Int*)self;
	if(p-> x < 2) return 1;

	int (*fib)(Class*) = ((Fib2*)Fib2_v->data[p->id])->fib;

	Int p1 = {Int_classId, p->x - 2};
	Int p2 = {Int_classId, p->x - 1};

	return ((Fib2*)Fib2_v->data[p->id])->fib((Class*)&p1) +
			((Fib2*)Fib2_v->data[p->id])->fib((Class*)&p2);
}

Fib2* newFib2_Int() {
	Fib2 *impl = new Fib2();
	setVec(Fib2_v, Int_classId, (void*)impl);
	impl->fib = &Fib2_Int_fib;
	return impl;
}
Fib2* Fib2_Int_ = newFib2_Int();

int main() {
  long start;

  start = gett();


	Int *p = newInt(40);

	printf("fib %d\n", ((Fib2*)Fib2_v->data[p->id])->fib((Class*)p));
	delete p;

  printf("%ld\n", gett() - start);

	return 0;
}
