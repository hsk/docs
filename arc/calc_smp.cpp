// calc_smp.cpp
#include <memory>
#include <iostream>

#define m_case(e2,T,e1) if(shared_ptr<T> e1 = dynamic_pointer_cast<T>(e2))

using namespace std;

struct e {
	virtual void dummy(){}
};

typedef shared_ptr<e> E;

struct eint : e {
	int i;
	eint(int i):i(i){}
};
E EInt(int i) { return E(new eint(i)); }
struct eadd : e {
	E l;
	E r;
	eadd(E l, E r):l(l),r(r) {}
};
E EAdd(E l, E r) { return E(new eadd(l, r)); }

int eval(E e) {
	m_case(e, eint, e1) return e1->i;
	m_case(e, eadd, e1) return eval(e1->l) + eval(e1->r);
	return 0;
}

int main() {
	E e2 = E(new eadd(E(new eint(1)),E(new eint(2))));
	E e = EAdd(EInt(1),EInt(2));
	cout << "result=" << eval(e) << endl;
    return 0;
}
