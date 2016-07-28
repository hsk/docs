#include "ast.h"
#include <cassert>
#include <exception>

typedef std::pair<std::string, T*> dest_t;

class Not_found : public std::exception {};

// auxiliary function for Call
static svec_t target_args(std::string src, svec_t all, svec_t args) {
	svec_t rc;
	int n = 0;
	for (auto y : args) {
		if (src == y) rc.push_back(all[n]);
		n++;
	}
	return rc;
}

static std::pair<bool, svec_t> target_e(std::string src, dest_t dest, E* e);

// for register coalescing
// [XXX] Callがあったら、そこから先は無意味というか逆効果なので追わない。
//         そのために「Callがあったかどうか」を返り値の第1要素に含める。
static std::pair<bool, svec_t> target_exp(std::string src, dest_t dest, Exp* exp) {
	if (auto exp_ = dynamic_cast<Mov*> (exp)) {
		if (exp_->id == src && is_reg(dest.first)) {
			assert(!(dynamic_cast<Unit*>(dest.second)));
			return std::pair<bool, svec_t>(false, svec_t{dest.first});
		}
	}
	if (auto exp_ = dynamic_cast<IfEq*> (exp)) {
		auto r1 = target_e(src, dest, exp_->e1.get());
		auto r2 = target_e(src, dest, exp_->e2.get());
		return std::pair<bool, svec_t>(r1.first && r2.first, insert<std::string>(r1.second,r2.second));
	}
	if (auto exp_ = dynamic_cast<IfLE*> (exp)) {
		auto r1 = target_e(src, dest, exp_->e1.get());
		auto r2 = target_e(src, dest, exp_->e2.get());
		return std::pair<bool, svec_t>(r1.first && r2.first, insert<std::string>(r1.second,r2.second));
	}
	if (auto exp_ = dynamic_cast<IfGE*> (exp)) {
		auto r1 = target_e(src, dest, exp_->e1.get());
		auto r2 = target_e(src, dest, exp_->e2.get());
		return std::pair<bool, svec_t>(r1.first && r2.first, insert<std::string>(r1.second,r2.second));
	}
	/*
	  | CallCls(x, ys) ->
		  let ys = target_args src regs 0 ys in
		  true, (ys @ if x = src then [reg_cl] else [])
	 */
	if (auto exp_ = dynamic_cast<Call*>(exp)) {
		auto ys = target_args(src, regs, exp_->ids);
		return std::pair<bool, svec_t>(true, ys);
	}
	return std::pair<bool, svec_t>(false, svec_t());
}

// register targeting
static std::pair<bool, svec_t> target_e(std::string src, dest_t dest, E* e) {
	if (auto e_ = dynamic_cast<Ans*> (e))
		return target_exp(src, std::move(dest), e_->exp.get());
	if (auto e_ = dynamic_cast<Let*> (e)) {
		auto crs1 = target_exp(src, dest_t(e_->id, e_->t.get()), e_->exp.get());
		if (crs1.first) return crs1;
		auto crs2 = target_e(src, dest, e_->e.get());
		crs2.second = insert<std::string>(crs1.second, crs2.second);
		return crs2;
	}
	assert(false);
}

static svec_t source_exp(T* t, Exp* exp);

// "register sourcing" (?) as opposed to register targeting
// （x86の2オペランド命令のためのregister coalescing
static svec_t source_e(T* t, E* e) {
	if (auto e_ = dynamic_cast<Ans*> (e)) return source_exp(t, e_->exp.get());
	if (auto e_ = dynamic_cast<Let*> (e)) return source_e(t, e_->e.get());
	assert(false);
}

static svec_t source_exp(T* t, Exp* exp) {
	if (auto exp_ = dynamic_cast<Mov*> (exp)) return svec_t {exp_->id};
	if (auto exp_ = dynamic_cast<Neg*> (exp)) return svec_t {exp_->id};
	if (auto exp_ = dynamic_cast<Sub*> (exp)) return svec_t {exp_->id};
	if (auto exp_ = dynamic_cast<Add*> (exp)) {
		auto imm = exp_->imm.get();
		if (auto im = dynamic_cast<V*> (imm)) return svec_t {exp_->id, im->v};
		return svec_t{exp_->id};
	}
	if (auto exp_ = dynamic_cast<IfEq*> (exp))
		return insert<std::string>(source_e(t, exp_->e1.get()),source_e(t, exp_->e2.get()));
	if (auto exp_ = dynamic_cast<IfLE*> (exp))
		return insert<std::string>(source_e(t, exp_->e1.get()),source_e(t, exp_->e2.get()));
	if (auto exp_ = dynamic_cast<IfGE*> (exp))
		return insert<std::string>(source_e(t, exp_->e1.get()),source_e(t, exp_->e2.get()));
	if (auto exp_ = dynamic_cast<Call*> (exp)) {
		if (!dynamic_cast<Unit*> (t)) return svec_t {regs[0]};
		else return svec_t();
	}
	return svec_t();
}

// allocにおいてspillingがあったかどうかを表すデータ型
class Alloc_result {
public:
	std::string id;
	Alloc_result(std::string id) : id(id) {}
	virtual ~Alloc_result() {}
};
typedef std::unique_ptr<Alloc_result> UAlloc_result;

class Alloc : public Alloc_result {
public:
	Alloc(std::string id) : Alloc_result(id) {}
};
UAlloc_result UAlloc(std::string id) { return UAlloc_result(new Alloc(id)); }

class Spill : public Alloc_result {
public:
	Spill(std::string id) : Alloc_result(id) {}
};
UAlloc_result USpill(std::string id) { return UAlloc_result(new Spill(id)); }

UAlloc_result alloc(E* cont, regenv_t regenv, std::string x, T* t, svec_t prefer) {
	// allocate a register or spill a variable
	assert(regenv.find(x) == regenv.end());
	svec_t all;
	if (!dynamic_cast<Unit*> (t)) all.insert(all.end(), allregs.begin(), allregs.end());
	if (all.empty()) return UAlloc("%unit");
	if (is_reg(x)) return UAlloc(x);
	auto free = fv(cont);
	std::set<std::string> live; // 生きているレジスタ
	for (auto y : free) {
		if (is_reg(y)) live.insert(y);
		else {
			auto it = regenv.find(y);
			if (it != regenv.end()) live.insert(it->second);
		}
	}
	// そうでないレジスタを探す
	for (auto r : prefer) if (live.find(r) == live.end()) return UAlloc(r);
	for (auto r : all) if (live.find(r) == live.end()) return UAlloc(r);		
	fprintf(stderr, "register allocation failed for %s\n", x.c_str());
	// 型の合うレジスタ変数を探す
	for (auto y : free)
		if (!is_reg(y) && find(all.begin(), all.end(), regenv.find(y)->second) != all.end()) {
			fprintf(stderr, "spilling %s from %s\n", y.c_str(), regenv.find(y)->second.c_str());
			return USpill(y);
		}
	assert(false);
}

// auxiliary function for walk_e and walk_exp_and_restore
static regenv_t add(std::string x, std::string r, regenv_t regenv) {
	if (is_reg(x)) {
		assert(x == r);
		return regenv;
	}
	regenv_t rc = regenv;
	rc[x] = r;
	return rc;
}

// auxiliary functions for walk_exp
class NoReg : public std::exception {
public:
	std::string id;
	UT ty;
	NoReg(std::string id, UT ty) : id(id), ty(std::move(ty)) {}
};

static std::string find_x(std::string x, UT t, regenv_t regenv) {
	if (is_reg(x)) return x;
	auto it = regenv.find(x);
	if (it != regenv.end()) return it->second;
	throw NoReg(x, std::move(t));
}

static UId_or_imm find_imm(Id_or_imm* x, regenv_t regenv) {
	if (auto im = dynamic_cast<V*> (x)) return UV(find_x(im->v, UInt(), regenv));
	if (auto im = dynamic_cast<C*> (x)) return UC(im->i);
	assert(false);
}

static wpair walk_e(dest_t dest, E* cont, regenv_t regenv, UE e);

std::string show_regenv(regenv_t revenv) {
	svec_t r;
	for (auto it : revenv) r.push_back(it.first + ":" + it.second);
	return concat(", ", r);
}

// ifのレジスタ割り当て
static wpair walk_exp_if(dest_t dest, E* cont, regenv_t regenv, std::function<UExp(UE, UE)> constr, UE e1, UE e2) {

	auto p1 = walk_e(dest, cont, regenv, std::move(e1));
	auto p2 = walk_e(dest, cont, regenv, std::move(e2));
	auto regenv_ = regenv_t(); // 両方に共通のレジスタ変数だけ利用
	for (auto x : fv(cont)) {
		if (is_reg(x)) continue;
		auto r1 = p1.second.find(x);
		auto r2 = p2.second.find(x);
		if (r1->second != r2->second) continue;
		regenv_[x] = r1->second;
	}
	// そうでない変数は分岐直前にセーブ
	auto e = UAns(constr(std::move(p1.first), std::move(p2.first)));
	for (auto x : fv(cont)) {
		if (x == dest.first || regenv.find(x) == regenv.end()
				|| regenv_.find(x) != regenv_.end()) continue;
		e = UE(seq(USave(regenv.find(x)->second, x), std::move(e)));
	}
	return wpair(std::move(e), regenv_);
}

// 関数呼び出しのレジスタ割り当て
static wpair
walk_exp_call(dest_t dest, E* cont, regenv_t regenv, std::function<UExp(svec_t)> constr, svec_t ys) {
	auto ys2 = svec_t();
	for (auto y : ys) ys2.push_back(find_x(y, UInt(), regenv));
	auto e = UAns(constr(ys2));
	for (auto x : fv(cont)) {
		auto it = regenv.find(x);
		if (x == dest.first && it == regenv.end()) continue;
		e.reset(seq(USave(it->second, x), std::move(e)));
	}
	return wpair(std::move(e), regenv_t());
}

// 各命令のレジスタ割り当て
static wpair
walk_exp(dest_t dest, E* cont, regenv_t regenv, UExp exp) {
	Exp* expp = exp.get();
	if (dynamic_cast<Nop*> (expp) || dynamic_cast<Set*> (expp) || dynamic_cast<Restore*> (expp) //|| dynamic_cast<Comment*>(expp)
			)
		return wpair(UAns(std::move(exp)), regenv);
	if (auto e_ = dynamic_cast<Mov*> (expp))
		return wpair(UAns(UMov(find_x(e_->id, UInt(), regenv))), regenv);
	if (auto e_ = dynamic_cast<Neg*> (expp))
		return wpair(UAns(UNeg(find_x(e_->id, UInt(), regenv))), regenv);
	if (auto e_ = dynamic_cast<Add*> (expp)) {
		auto x = find_x(e_->id, UInt(), regenv);
		auto y = find_imm(e_->imm.get(), regenv);
		return wpair(UAns(UAdd(x, std::move(y))), regenv);
	}
	if (auto e_ = dynamic_cast<Sub*> (expp)) {
		auto x = find_x(e_->id, UInt(), regenv);
		auto y = find_imm(e_->imm.get(), regenv);
		return wpair(UAns(USub(x, std::move(y))), regenv);
	}
	if (auto e_ = dynamic_cast<Ld*> (expp)) {
		auto x = find_x(e_->id, UInt(), regenv);
		auto y = find_imm(e_->imm.get(), regenv);
		return wpair(UAns(ULd(x, std::move(y), e_->i)), regenv);
	}
	if (auto e_ = dynamic_cast<St*> (expp)) {
		auto x = find_x(e_->id, UInt(), regenv);
		auto y = find_imm(e_->imm.get(), regenv);
		auto z = find_x(e_->id2, UInt(), regenv);
		return wpair(UAns(USt(x, z, std::move(y), e_->i)), regenv);
	}
	if (auto e_ = dynamic_cast<IfEq*> (expp))
		return walk_exp_if(dest, cont, regenv, [=](UE e1, UE e2) {
			return UIfEq(find_x(e_->id, UInt(), regenv), find_imm(e_->imm.get(), regenv), std::move(e1), std::move(e2));
		}, std::move(e_->e1), std::move(e_->e2));
	if (auto e_ = dynamic_cast<IfLE*> (expp))
		return walk_exp_if(dest, cont, regenv, [=](UE e1, UE e2) {
			return UIfLE(find_x(e_->id, UInt(), regenv), find_imm(e_->imm.get(), regenv), std::move(e1), std::move(e2));
		}, std::move(e_->e1), std::move(e_->e2));
	if (auto e_ = dynamic_cast<IfGE*> (expp))
		return walk_exp_if(dest, cont, regenv, [=](UE e1, UE e2) {
			return UIfGE(find_x(e_->id, UInt(), regenv), find_imm(e_->imm.get(), regenv), std::move(e1), std::move(e2));
		}, std::move(e_->e1), std::move(e_->e2));
	if (auto e_ = dynamic_cast<Call*> (expp))
		return walk_exp_call(dest, cont, regenv, [=](auto ys) {
			return UCall(e_->id, std::move(ys));
		}, e_->ids);

	assert(false);
}

// 使用される変数をスタックからレジスタへRestore
static wpair
walk_exp_and_restore(dest_t dest, E* cont, regenv_t regenv, UExp exp) {
	try {
		return walk_exp(dest, cont, regenv, UExp(exp->clone()));
	} catch (NoReg& a) { //(x, t) ->
		fprintf(stderr, "restoring %s\n", dest.first.c_str());
		return walk_e(dest, cont, regenv, ULet(a.id, std::move(a.ty), URestore(a.id), UAns(std::move(exp))));
	}
}

// 命令列のレジスタ割り当て
static wpair walk_e(dest_t dest, E* cont, regenv_t regenv, UE e) {
	if (auto e_ = dynamic_cast<Ans*> (e.get()))
		return walk_exp_and_restore(dest, cont, regenv, std::move(e_->exp));
	if (auto e_ = dynamic_cast<Let*> (e.get())) {
		//  | Let((x, t) as xt, exp, e) ->
		if (regenv.find(e_->id) != regenv.end())
			fprintf(stderr, "find error %s -> %s\n", e_->id.c_str(), regenv.find(e_->id)->second.c_str());
		assert(regenv.find(e_->id) == regenv.end());
		auto cont0 = concat(UE(e_->e->clone()), dest.first, UT(dest.second->clone()), UE(cont->clone()));
		auto r1 = walk_exp_and_restore(dest_t(e_->id, e_->t.get()), cont0.get(), regenv, std::move(e_->exp));
		// (e1_, regenv1)
		auto targets = target_e(e_->id, dest, cont0.get());
		auto sources = source_e(e_->t.get(), r1.first.get());
		// レジスタ間のmovよりメモリを介するswapのほうが問題なので、sourcesよりtargetsを優先
		targets.second.insert(targets.second.end(), sources.begin(), sources.end());
		auto ralloc = alloc(cont0.get(), r1.second, e_->id, e_->t.get(), targets.second);
		if (auto res = dynamic_cast<Spill*> (ralloc.get())) {
			auto y = res->id;
			auto it = r1.second.find(y);
			if (it == r1.second.end()) throw Not_found();
			auto r = it->second;
			auto regenv2 = regenv_t();
			regenv2.insert(r1.second.begin(), r1.second.end());
			regenv2.erase(y);
			auto r2 = walk_e(dest, cont, add(e_->id, r, regenv2), std::move(e_->e));
			auto it2 = regenv.find(y);
			UExp save = it2 != regenv.end() ? USave(it2->second, y) : UNop();
			return wpair(UE(seq(std::move(save),
					concat(std::move(r1.first), r, std::move(e_->t), std::move(r2.first)))), std::move(r1.second));
		}
		if (auto res = dynamic_cast<Alloc*> (ralloc.get())) {
			auto r2 = walk_e(dest, cont, add(e_->id, res->id, r1.second), std::move(e_->e));
			return wpair(concat(std::move(r1.first), res->id, std::move(e_->t), std::move(r2.first)), std::move(r1.second));
		}
	}
	assert(false);
}

// 関数のレジスタ割り当て
static UFundef walk_fundef(UFundef fundef) {
	auto regenv = regenv_t{{fundef->name, reg_cl}};
	int i = 0;
	auto arg_regs = svec_t();
	// 今のレジスタ割り当てを表す（変数からレジスタへの）写像regenv
	for (auto y : fundef->args) {
		assert(!is_reg(y));
		auto r = regs[i];
		i++;
		arg_regs.push_back(r);
		regenv[y] = r;
	}
	auto a = regs[0];
	if (dynamic_cast<Unit*> (fundef->ret.get())) a = gentmp(fundef->ret.get());
	auto r = walk_e(dest_t(a, fundef->ret.get()),
			UAns(UMov(a)).get(), regenv, std::move(fundef->body));
	return uFundef(fundef->name, arg_regs,
			std::move(r.first), std::move(fundef->ret));
}

// プログラム全体のレジスタ割り当て
static UProg walk_prog(UProg prog) {
	fprintf(stderr, "register allocation: may take some time (up to a few minutes, depending on the size of functions)\n");
	auto fundefs = std::vector<std::unique_ptr < Fundef >> ();
	for (auto& fundef : prog->fundefs)
		fundefs.push_back(walk_fundef(std::move(fundef)));
	auto e = UAns(UNop());
	auto r = walk_e(dest_t(gentmp(UUnit().get()), UUnit().get()), e.get(), regenv_t(), std::move(prog->e));
	return uProg(std::move(fundefs), std::move(r.first));
}

UProg regAlloc(UProg prog) {
	return walk_prog(std::move(prog));
}
