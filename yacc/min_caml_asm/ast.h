#include <stdio.h>
#include <vector>
#include <set>
#include <map>
#include <memory>
#include <string>


typedef std::unique_ptr<std::string> UStr;
typedef std::map<std::string, std::string> regenv_t;
typedef std::vector<std::string> svec_t;

class T {
public:
  virtual ~T() {}
  virtual T* clone() = 0;
};
typedef std::unique_ptr<T> UT;

class Unit : public T {
  T* clone() { return new Unit(); }
};
inline UT UUnit() { return UT(new Unit()); }

class Bool : public T {
  T* clone() { return new Bool(); }
};
inline UT UBool() { return UT(new Bool()); }

class Int : public T {
  T* clone() { return new Int(); }
};
inline UT UInt() { return UT(new Int()); }

class Fun : public T {
public:
	std::vector<UT> ts;
	UT t;

	Fun(std::vector<UT> ts, UT t)
	: ts(std::move(ts)), t(std::move(t)) {
		ts.~vector();
	}
	T* clone() {
		std::vector<UT> ts2;
		for (auto& t1 : ts) ts2.push_back(UT(t1->clone()));
		return new Fun(std::move(ts2), UT(t->clone()));
	}
};

class Id_or_imm {
public:
  virtual ~Id_or_imm() {}
  virtual Id_or_imm* clone() = 0;
};
typedef std::unique_ptr<Id_or_imm> UId_or_imm;

class V : public Id_or_imm {
public:
	std::string v;

	V(std::string v) : v(v) {
	}
    Id_or_imm* clone() {
		return new V(v);
	}
};

class C : public Id_or_imm {
public:
	int i;

	C(int i) : i(i) {
	}
    Id_or_imm* clone() {
		return new C(i);
	}
};

class Exp {
public:
	virtual ~Exp() {}
	virtual Exp* clone() = 0;
};
typedef std::unique_ptr<Exp> UExp;

class E {
public:
	virtual ~E() {}
	virtual E* clone() = 0;
};
typedef std::unique_ptr<E> UE;
typedef std::pair<UE, regenv_t> wpair;

class Ans : public E {
public:
	UExp exp;
	Ans(UExp exp) : exp(std::move(exp)) {}
	E* clone() { return new Ans(UExp(exp->clone())); }
};
inline UE UAns(UExp exp) { return UE(new Ans(std::move(exp))); }

class Let : public E {
public:
	std::string id;
	UT t;
	UExp exp;
	UE e;

	Let(std::string id, UT t, UExp exp, UE e)
	: id(id), t(std::move(t)), exp(std::move(exp)), e(std::move(e)) {}
	E* clone() { return new Let(id, UT(t->clone()), UExp(exp->clone()), UE(e->clone())); }
};
inline UE ULet(std::string id, UT t, UExp exp, UE e) { return UE(new Let(id, std::move(t), std::move(exp), std::move(e))); }

class Nop : public Exp {
public:
  Nop() {}
  Exp* clone() { return new Nop(); }
};

class Set : public Exp {
public:
	int v;

	Set(int v) : v(v) {
	}
    Exp* clone() { return new Set(v); }
};

class SetL : public Exp {
public:
	std::string l;

	SetL(std::string l) : l(l) {
	}
    Exp* clone() { return new SetL(l); }
};

class Mov : public Exp {
public:
	std::string id;

	Mov(std::string id) : id(id) {}
    Exp* clone() { return new Mov(id); }
};

class Neg : public Exp {
public:
	std::string id;

	Neg(std::string id) : id(id) {
	}
    Exp* clone() { return new Neg(id); }
};

class Add : public Exp {
public:
	std::string id;
	std::unique_ptr<Id_or_imm> imm;

	Add(std::string id, std::unique_ptr<Id_or_imm> imm)
	: id(id), imm(std::move(imm)) {
	}
    Exp* clone() { return new Add(id,std::unique_ptr<Id_or_imm>(imm->clone())); }
};

class Sub : public Exp {
public:
	std::string id;
	std::unique_ptr<Id_or_imm> imm;

	Sub(std::string id, std::unique_ptr<Id_or_imm> imm)
	: id(id), imm(std::move(imm)) {
	}
    Exp* clone() { return new Sub(id,std::unique_ptr<Id_or_imm>(imm->clone())); }
};

class Ld : public Exp {
public:
	std::string id;
	std::unique_ptr<Id_or_imm> imm;
	int i;

	Ld(std::string id, std::unique_ptr<Id_or_imm> imm, int i)
	: id(id), imm(std::move(imm)), i(i) {
	}
    Exp* clone() { return new Ld(id,std::unique_ptr<Id_or_imm>(imm->clone()), i); }
};

class St : public Exp {
public:
	std::string id;
	std::string id2;
	std::unique_ptr<Id_or_imm> imm;
	int i;

	St(std::string id, std::string id2, std::unique_ptr<Id_or_imm> imm, int i)
	: id(id), id2(id2), imm(std::move(imm)), i(i) {
	}
    Exp* clone() { return new St(id,id2,std::unique_ptr<Id_or_imm>(imm->clone()), i); }
};

class IfEq : public Exp {
public:
	std::string id;
	std::unique_ptr<Id_or_imm> imm;
	UE e1;
	UE e2;
	IfEq(std::string id, std::unique_ptr<Id_or_imm> imm, UE e1, UE e2)
	: id(id), imm(std::move(imm)), e1(std::move(e1)),e2(std::move(e2)) {
	}
    Exp* clone() { return new IfEq(id,std::unique_ptr<Id_or_imm>(imm->clone()), UE(e1->clone()), UE(e2->clone())); }
};
class IfLE : public Exp {
public:
	std::string id;
	std::unique_ptr<Id_or_imm> imm;
	UE e1;
	UE e2;
	IfLE(std::string id, std::unique_ptr<Id_or_imm> imm, UE e1, UE e2)
	: id(id), imm(std::move(imm)), e1(std::move(e1)),e2(std::move(e2)) {
	}
    Exp* clone() { return new IfLE(id,std::unique_ptr<Id_or_imm>(imm->clone()), UE(e1->clone()), UE(e2->clone())); }
};
class IfGE : public Exp {
public:
	std::string id;
	std::unique_ptr<Id_or_imm> imm;
	UE e1;
	UE e2;
	IfGE(std::string id, std::unique_ptr<Id_or_imm> imm, UE e1, UE e2)
	: id(id), imm(std::move(imm)), e1(std::move(e1)),e2(std::move(e2)) {
	}
    Exp* clone() { return new IfGE(id,std::unique_ptr<Id_or_imm>(imm->clone()), UE(e1->clone()), UE(e2->clone())); }
};

class Call : public Exp {
public:
	std::string id;
	std::vector<std::string> ids;

	Call(std::string id, std::vector<std::string> ids)
	: id(id), ids(std::move(ids)) {}
    Exp* clone() { return new Call(id,ids); }
};

class Save : public Exp {
public:
	std::string id;
	std::string id2;

	Save(std::string id, std::string id2) : id(id), id2(id2) {
	}
    Exp* clone() { return new Save(id,id2); }
};

class Restore : public Exp {
public:
	std::string id;

	Restore(std::string id) : id(id) {
	}
    Exp* clone() { return new Restore(id); }
};

class Fundef {
public:
	std::string name;
	std::vector<std::string> args;
	UE body;
	UT ret;

	Fundef(std::string name, std::vector<std::string> args, UE body,
	UT ret)
	: name(name), args(args), body(std::move(body)), ret(std::move(ret)) {
	}
};
typedef std::unique_ptr<Fundef> UFundef;

class Prog {
public:
	std::vector<UFundef> fundefs;
	UE e;

	Prog(std::vector<UFundef> fundefs, UE e)
	: fundefs(std::move(fundefs)), e(std::move(e)) {
		fundefs.~vector();
	}
};
typedef std::unique_ptr<Prog> UProg;

std::string concat(std::string, svec_t);
std::string show_t(std::string, T*);
std::string show_e(std::string, E*);
std::string show_exp(std::string, Exp*);
std::string show_fundef(std::string, Fundef*);
std::string show_prog(Prog*);

inline E* seq(UExp e1, UE e2) {
  return new Let("_", UT(new Unit()), std::move(e1), std::move(e2));
}

extern svec_t regs;
extern std::set<std::string> allregs;

#define reg_cl "%edi"
#define reg_sp "%ebp"
#define reg_hp "min_caml_hp"

bool is_reg(std::string x);
svec_t remove_and_uniq(std::set<std::string> xs, svec_t ls);
int align(int i);
std::string genid(std::string s);
std::string gentmp(T* typ);
svec_t fv(E*);
UE concat(UE e1, std::string x, UT t, UE e2);

UProg simm(UProg);
UProg regAlloc(UProg);
void emit(FILE* oc, Prog* prog);

void yyerrer(const char *s, ...);
int parse_input(FILE*);
int parse_string(const char*);

template<class T1>
std::vector<T1> insert(std::vector<T1>v1,std::vector<T1>v2) {
	v1.insert(v1.end(),v2.begin(),v2.end());
	return v1;
}
