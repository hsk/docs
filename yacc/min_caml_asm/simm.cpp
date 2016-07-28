#include "ast.h"
#include <cassert>
#include <algorithm>

static
UExp walk_exp(std::map<std::string,int> env, UExp e);
// 命令列の即値最適化 (caml2html: simm13_g)

static
UE walk_e(std::map<std::string,int> env, UE e) {
	if(auto e_ = dynamic_cast<Ans*>(e.get()))
		return UAns(walk_exp(env, std::move(e_->exp)));
	if(auto e_ = dynamic_cast<Let*>(e.get())) {
		auto ep = std::move(e);
		// 即値なら
		if (auto e2_ = dynamic_cast<Set*>(e_->exp.get())) {
			auto ep2 = std::move(e_->exp);
			// 環境に保存して後続を計算
			std::map<std::string,int> env2=env;
			env2[e_->id]=e2_->v;
			auto e0 = walk_e(env2, std::move(e_->e));
			// 後続でレジスタを使っていれば、letを残すが、使っていなければ消す
			auto vec = fv(e0.get());
			if (std::find(vec.begin(),vec.end(), e_->id) != vec.end()) {
				return ULet(
						e_->id,
						std::move(e_->t), 
						UExp(new Set(e2_->v)),
						std::move(e0));
			}
			return e0;			
		}
		// 即値でなければ、そのまま
		return ULet(e_->id, std::move(e_->t), walk_exp(env, std::move(e_->exp)), walk_e(env, std::move(e_->e)));
	}
	assert(false);
}

// 各命令の即値最適化 (caml2html: simm13_gprime)
static
UExp walk_exp(std::map<std::string,int> env, UExp e) {
	// 環境に変数の即値があれば、即値に置き換える
	if(auto e_ = dynamic_cast<Add*>(e.get())) {
		if (auto v = dynamic_cast<V*>(e_->imm.get())) {
			auto it = env.find(v->v);
			if (it != env.end())
				return UExp(new Add(e_->id,std::unique_ptr<Id_or_imm>(new C(it->second))));
			auto it2 = env.find(e_->id);
			if (it2 != env.end())
				return UExp(new Add(v->v,std::unique_ptr<Id_or_imm>(new C(it2->second))));
			return e;
		}
	}
	if(auto e_ = dynamic_cast<Sub*>(e.get())) {
		if (auto v = dynamic_cast<V*>(e_->imm.get())) {
			auto it = env.find(v->v);
			if (it != env.end())
				return UExp(new Sub(e_->id, UId_or_imm(new C(it->second))));
			return e;
		}
	}
	if(auto e_ = dynamic_cast<Ld*>(e.get())) {
		if (auto v = dynamic_cast<V*>(e_->imm.get())) {
			auto it = env.find(v->v);
			if (it != env.end())
				return UExp(new Ld(e_->id, UId_or_imm(new C(it->second)),e_->i));
			return e;
		}
	}
	if(auto e_ = dynamic_cast<St*>(e.get())) {
		if (auto v = dynamic_cast<V*>(e_->imm.get())) {
			auto it = env.find(v->v);
			if(it!=env.end())
				return UExp(new St(e_->id, e_->id2, UId_or_imm(new C(it->second)),e_->i));
			return e;
		}
	}
	// 分岐命令の場合は両方を変換する
	if(auto e_ = dynamic_cast<IfEq*>(e.get())) {
		if(auto v = dynamic_cast<V*>(e_->imm.get())){
			auto it = env.find(v->v); 
			if (it != env.end())
				return UExp(new IfEq(e_->id, UId_or_imm(new C(it->second)),
						walk_e(env, std::move(e_->e1)), walk_e(env, std::move(e_->e2))));
			it = env.find(e_->id);
			if (it != env.end())
				return UExp(new IfEq(v->v, UId_or_imm(new C(it->second)),
						walk_e(env, std::move(e_->e1)), walk_e(env, std::move(e_->e2))));
		}
		return UExp(new IfEq(e_->id, std::move(e_->imm),
				walk_e(env, std::move(e_->e1)), walk_e(env, std::move(e_->e2))));
	}
	if(auto e_ = dynamic_cast<IfLE*>(e.get())) {
		if(auto v = dynamic_cast<V*>(e_->imm.get())){
			auto it = env.find(v->v); 
			if (it != env.end())
				return UExp(new IfLE(e_->id, UId_or_imm(new C(it->second)),
						walk_e(env, std::move(e_->e1)), walk_e(env, std::move(e_->e2))));
			it = env.find(e_->id);
			if (it != env.end())
				return UExp(new IfGE(v->v, UId_or_imm(new C(it->second)),
						walk_e(env, std::move(e_->e1)), walk_e(env, std::move(e_->e2))));
		}
		return UExp(new IfLE(e_->id, std::move(e_->imm),
				walk_e(env, std::move(e_->e1)), walk_e(env, std::move(e_->e2))));
	}
	if(auto e_ = dynamic_cast<IfGE*>(e.get())) {
		if(auto v = dynamic_cast<V*>(e_->imm.get())){
			auto it = env.find(v->v); 
			if (it != env.end())
				return UExp(new IfGE(e_->id, UId_or_imm(new C(it->second)),
						walk_e(env, std::move(e_->e1)), walk_e(env, std::move(e_->e2))));
			it = env.find(e_->id);
			if (it != env.end())
				return UExp(new IfLE(v->v, UId_or_imm(new C(it->second)),
						walk_e(env, std::move(e_->e1)), walk_e(env, std::move(e_->e2))));
		}
		return UExp(new IfGE(e_->id, std::move(e_->imm),
				walk_e(env, std::move(e_->e1)), walk_e(env, std::move(e_->e2))));
	}
    // 他の命令はそのまま
	return e;
}

// トップレベル関数の即値最適化
static
UFundef walk_fundef(UFundef fundef) {
	Fundef* f_ = fundef.get();
	return UFundef(new Fundef(f_->name,f_->args,walk_e(std::map<std::string,int>(), std::move(f_->body)),std::move(f_->ret)));
}

// プログラム全体の即値最適化
static
UProg walk_prog(UProg prog) {
	Prog* p_ = prog.get();
	auto v = std::vector<std::unique_ptr<Fundef>>();
	for(auto& fundef : p_->fundefs)
		v.push_back(walk_fundef(std::move(fundef)));
	return UProg(new Prog(
			std::move(v),
			walk_e(std::map<std::string,int>(),std::move(p_->e))));
}

UProg simm(UProg prog) {
	return walk_prog(std::move(prog));
}
