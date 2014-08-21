#include <stdio.h>
#include "arc.hpp"


struct A : arc::object {
	int a;
	A(int a):a(a) {
		printf("new a %d\n",a);
	}
	virtual ~A() {
		printf("delete a %d\n",a);
	}
};

struct B : arc::object {

	arc::ptr<A> a;
	arc::ptr<A> b;
	B():a(new A(60000)),b(NULL) {
		printf("new b\n");
	}
	virtual ~B() {
		printf("delete b\n");
	}
};

A* getA(int a) {
	arc::LocalPool p;
	arc_return(new A(a));
}

arc_instance int main() {

	B* b = new B();
	printf("enter local pool\n");
	{
		arc::AutoReleasePool ap;
		getA(2222);
		arc::LocalPool p;
		b->b = new A(111);
		b->b = getA(333);
		printf("leave start\n");
	}
	printf("leave local pool\n");

	return 0;
}
