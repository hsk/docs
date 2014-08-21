# C++ ARCライブラリ

## ARCとは

基本的には、allocしたら、ローカルスコープのプールに追加される。
returnではオートリリースプールに追加される。

強い参照と弱い参照を持つ。弱い参照は借り物のポインタなので参照カウンタを操作しない。
メモリプールから取得して、まとめてメモリプールを開放するような事もする。
関数のリターン時には自動的に参照カウンタをインクリメントし、
戻り値はオートリリースプールに入れる。
allocされた変数はスコープを抜けるとデクリメントされる。

オートリリースプールの良い点は、メモリの解放を自動で行う事が出来るようにする事である。

## ARCを使ってみる

```
// test_arc.m

#import <Foundation/Foundation.h>

@interface A : NSObject {
    @private int n;
}

-(id)initWithN:(int)n;
-(void)dealloc;

@end

@implementation A

-(id)initWithN:(int)n1 {
    self = [super init];
    if(!self) return self;
    n = n1;
    return self;
}

-(void)dealloc {
    NSLog(@"dealloc %d", self->n);
}

@end

A* test() {
    A *a1 = [[A alloc]initWithN:1];

    // これはスコープを抜けると消える。
    A *a2 = [[A alloc]initWithN:2];
    
    return a1;
}

void test2() {
    // この*aはオートリリースプールに含まれる。
    A *a = test();
}

int main() {
    @autoreleasepool {
        test2();
    }
    return 0;
}
```

## C++でARCを実装してみる。

以下のようなC++のプログラムでうまくライブラリを作ってみます:

```
// test_arc.cpp
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
```

## arc.hpp

C++でARCをする為のライブラリです。

```
// arc.hpp
namespace arc {

	class object {
	private:
		int ref_cnt;

	public:
		explicit object();
		virtual ~object() {}

		object* addRef() {
			ref_cnt++;
			return this;
		}

		void release() {
			ref_cnt--;
			if (ref_cnt == 0) {
				delete this;
			}
		}
	};

	class Pool {
		Pool* next;
		object* t;
		Pool* stack;
	public:

		Pool(object* t=NULL):t(t),next(NULL),stack(NULL) {
			if(t!=NULL)t->addRef();
		}

		object* add(object* t) {
			if(this->t==NULL) {
				t->addRef();
				this->t = t;
				return t;
			}
			Pool* p = new Pool(t);
			p->next = this->next;
			this->next=p;
			return t;
		}

		void release(object* t) {
			if(this->t == t) {
				t->release();
				this->t = NULL;
				return;
			}
			if(this->next != NULL) next->release(t);
		}

		~Pool() {
			if(t) {
				t->release();
			}
			if(next) delete next;
			if(stack) delete stack;
		}

		void push() {
			Pool* p = new Pool();
			p->t = t;
			p->next = next;
			p->stack = stack;
			stack = p;
			t = NULL;
			next = NULL;
		}
		void pop() {
			if(t) {
				t->release();
				t=NULL;
			}
			if(next) delete next;


			t = stack->t;
			next = stack->next;

			Pool* _stack = stack->stack;
			stack->t = NULL;
			stack->next = NULL;
			stack->stack = NULL;
			delete stack;

			stack = _stack;
		}
	};


	extern Pool localPool;

	object::object():ref_cnt(0) {
		localPool.add(this);
	}

	template <class T> struct ptr {
		T* o;
		explicit ptr(T* o):o(o) {
			if(o != NULL) {
				o->addRef();
				localPool.release(o);
			}
		}
		virtual ~ptr() {
			if(o) o->release();
		}
		T* operator->() {
			return o;
		}
		T* operator=(T* t) {
			if(o)o->release();
			t->addRef();
			return o = t;
		}
		T* operator()() {
			return o;
		}
	};

	struct LocalPool {
		LocalPool() {
			localPool.push();
		}
		~LocalPool() {
			localPool.pop();
		}
	};

	extern Pool autoPool;
	struct AutoReleasePool {
		AutoReleasePool() {
			autoPool.push();
		}
		~AutoReleasePool() {
			autoPool.pop();
		}
	};

	template <class T> T* ret(T* t) {
		autoPool.add(t);
		return t;
	}

}

#define arc_return(r) return arc::ret(r)
#define arc_case(e,t,e1) if(t e1 = dynamic_cast<t>(e))

#define arc_instance namespace arc {Pool localPool;Pool autoPool;}
```

## cppのスマートポインタを使ってみる

```
// test_smp.cpp

#import <iostream>
#import <memory>

struct a{
  int n;
  a(int n):n(n){}
  ~a() {
    std::cout << "delete " << n << std::endl;
  }
};

typedef std::shared_ptr<a> A;

A test() {
    A a1 = A(new a(1));
    A a2 = A(new a(2));
    return a1;
}

void test2() {
    A a = test();
}

int main() {
    test2();
    return 0;
}
```

# 加算インタプリタ

色々仕組みが出来たので、実際に使ってみましょう。

## Objective Cで加算インタプリタ

```
// arc_calc.m

#import <Foundation/Foundation.h>

@interface Exp : NSObject
@end

@implementation Exp
@end


@interface EInt : Exp {
	int i;
}
-(id)initWithInt:(int)i;
-(int)getInt;
-(void)dealloc;
@end

@implementation EInt
-(id)initWithInt:(int)i_ {
	self = [super init];
	if (!self) return self;
	self->i = i_;
	return self;
}
-(int)getInt {
	return self->i;
}
-(void)dealloc {
    NSLog(@"dealloc EInt");
}
@end

@interface EAdd : Exp {
	Exp* l;
	Exp* r;
}
-(Exp*)getL;
-(Exp*)getR;
-(id)initWithL:(Exp*)l R:(Exp*)r;
-(void)dealloc;
@end

@implementation EAdd
-(id)initWithL:(Exp*)l_ R:(Exp*)r_ {
	self = [super init];
	if (!self) return self;
	self->l = l_;
	self->r = r_;
	return self;
}
-(Exp*)getL {
	return self->l;
}
-(Exp*)getR {
	return self->r;
}
-(void)dealloc {
    NSLog(@"dealloc EAdd");
}
@end

int eval(Exp* exp) {
	if ([exp class] == [EInt class]) {
		return [(EInt*) exp getInt];
	}
	if ([exp class] == [EAdd class]) {
		EAdd* i = (EAdd*) exp;
		return eval([i getL]) + eval([i getR]);
	}
	return 0;
}

int main() {
	@autoreleasepool {
		EInt* i = [[EInt alloc] initWithInt: 1];
		EAdd* add = [[EAdd alloc] initWithL:i R:[[EInt alloc] initWithInt: 2] ];
		NSLog(@"result=%d", eval(add));
	    return 0;
	}
}
```


## C++ ARCで加算インタプリタ

```
// arc_calc.cpp
#include <iostream>
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

int eval(E* e) {
	arc_case(e,EInt*,e1) return e1->i;
	arc_case(e,EAdd*,e1) return eval(e1->l()) + eval(e1->r());
	return 0;
}

using namespace std;

arc_instance
int main() {
	E* e = new EAdd(new EInt(1), new EInt(2)));
	cout << "result=" << eval(e) << endl;
	return 0;
}
```

ARCを使えば、クラス(構造体)そのものがスマートポインタなので、包む必要がありません。
ただし、メンバ変数として保存する場合に、強参照する必要がある場合は、arc::ptrを使う必要があります。
基本生のポインタを使うので、arc::ptrの値を使うには、()オペレータを使って生のデータを取り出して扱います。

## C++のスマートポインタで加算インタプリタ

```
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
```

C++の場合は、スマートポインタを使うのが一般的なので、比較の為に、
スマートポインタを使ってみました。
スマートポインタであるEで包む必要があるのが面倒くさいので、補助関数のeint,eaddを作って使うとよいでしょう。
