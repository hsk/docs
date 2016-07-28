#include "ast.h"

svec_t regs = {"%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi"};
std::set<std::string> allregs = {"%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi"};

bool is_reg(std::string x) {
	return x.c_str()[0] == '%' || x == reg_hp;
}

svec_t
remove_and_uniq(std::set<std::string> xs, svec_t ls) {
	svec_t rc;
	for (auto l : ls) {
		auto it = xs.find(l);
		if (xs.end() != it) continue;
		xs.insert(l);
		rc.push_back(l);		
	}
	return rc;
}

static
svec_t fv_id_or_imm(Id_or_imm* imm) {
	if (auto im = dynamic_cast<V*> (imm))
		return svec_t{im->v};
	return svec_t();
}

static
svec_t fv_exp(Exp* e) {
	if (auto e_ = dynamic_cast<Mov*> (e)) return svec_t{e_->id};
	if (auto e_ = dynamic_cast<Neg*> (e)) return svec_t{e_->id};
	if (auto e_ = dynamic_cast<Save*> (e)) return svec_t{e_->id};
	if (auto e_ = dynamic_cast<Add*> (e))
		return insert<std::string>(svec_t{e_->id},fv_id_or_imm(e_->imm.get()));
	if (auto e_ = dynamic_cast<Sub*> (e))
		return insert<std::string>(svec_t{e_->id},fv_id_or_imm(e_->imm.get()));
	if (auto e_ = dynamic_cast<Ld*> (e))
		return insert<std::string>(svec_t{e_->id},fv_id_or_imm(e_->imm.get()));
	if (auto e_ = dynamic_cast<St*> (e))
		return insert<std::string>(svec_t{e_->id,e_->id2},fv_id_or_imm(e_->imm.get()));
	if (auto e_ = dynamic_cast<Call*> (e)) return e_->ids;
	if (auto e_ = dynamic_cast<IfEq*> (e)) {
		auto v = insert<std::string>(svec_t{e_->id}, fv_id_or_imm(e_->imm.get()));
		auto v2 = insert<std::string>(fv(e_->e1.get()), fv(e_->e2.get()));
		return insert<std::string>(v,remove_and_uniq(std::set<std::string>(), v2));
	}
	if (auto e_ = dynamic_cast<IfLE*> (e)) {
		auto v = insert<std::string>(svec_t{e_->id}, fv_id_or_imm(e_->imm.get()));
		auto v3 = insert<std::string>(fv(e_->e1.get()), fv(e_->e2.get()));
		return insert<std::string>(v, remove_and_uniq(std::set<std::string>(), v3));
	}
	if (auto e_ = dynamic_cast<IfGE*> (e)) {
		auto v = insert<std::string>(svec_t{e_->id}, fv_id_or_imm(e_->imm.get()));
		auto v2 = insert<std::string>(fv(e_->e1.get()), fv(e_->e2.get()));
		return insert<std::string>(v,remove_and_uniq(std::set<std::string>(), v2));
	}
	return svec_t();
}

static
svec_t fv_e(E* e) {
	if (auto e_ = dynamic_cast<Ans*> (e))
		return fv_exp(e_->exp.get());
	if (auto e_ = dynamic_cast<Let*> (e))
		return insert<std::string>(
			fv_exp(e_->exp.get()),
			remove_and_uniq(std::set<std::string>{e_->id}, fv_e(e_->e.get())));
	return svec_t();
}

svec_t fv(E* e) {
	return remove_and_uniq(std::set<std::string>(), fv_e(e));
}

UE concat(UE e1, std::string x, UT t, UE e2) {
	if (auto e = dynamic_cast<Ans*> (std::move(e1).get()))
		return ULet(x, std::move(t), std::move(e->exp), std::move(e2));
	if (auto e = dynamic_cast<Let*> (std::move(e1).get()))
		return ULet(e->id, std::move(e->t), std::move(e->exp), concat(std::move(e->e), x, std::move(t), std::move(e2)));
	return NULL;
}

int align(int i) {
	if (i % 8 == 0) return i;
	else return i + 4;
}

int counter = 0;

std::string genid(std::string s) {
	counter++;
	return s + "." + std::to_string(counter);
}

std::string id_of_typ(T* t) {
	if(dynamic_cast<Unit*>(t)) return "u";
	if(dynamic_cast<Bool*>(t)) return "b";
	if(dynamic_cast<Int*>(t)) return "i";
	if(dynamic_cast<Fun*>(t)) return "f";
	return "";
}

std::string gentmp(T* typ) {
    counter++;
    return std::string("T") + id_of_typ(typ) + std::to_string(counter);
}
