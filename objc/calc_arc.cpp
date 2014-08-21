#include <stdio.h>
#include "arc.hpp"

struct E : arc::object {};
struct EInt : E {
	int i;
	EInt(int i):i(i){}
};

struct EAdd : E {
	arc::ptr<E> l;
	arc::ptr<E> r;
	EAdd(E* l, E* r):l(l),r(r){}
};

struct ESym : E {
	char* id;
	ESym(char* id) {
		id=id;
	}
	~ESym() {
		delete [] id;
	}
};

int eval(E* e) {
	arc_case(e,EInt*,e1) return e1->i;
	arc_case(e,EAdd*,e1) return eval(e1->l()) + eval(e1->r());
	return 0;
}

E* getE() {
	arc::LocalPool p;
	E* e = new EAdd(new EInt(1), new EInt(2));
	arc_return(e);
}

arc_instance
int main() {
	{arc::AutoReleasePool ap;

		{arc::LocalPool p;
			E* e = getE();
			printf("%d\n", eval(e));
		}
	}
	return 0;
}
