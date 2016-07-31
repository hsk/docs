#include "ast.h"
#include <cassert>
#include <algorithm>

typedef std::map<std::string,int> env_t;

static UE walk_e(env_t env, UE e);

static UExp walk_exp_if(env_t env, std::string id, UId_or_imm imm, UE e1, UE e2, std::function<UExp(std::string,UId_or_imm,UE,UE)> f) {
	if(auto v = dynamic_cast<V*>(imm.get())){
		auto it = env.find(v->v); 
		if (it != env.end())
			return f(id, UC(it->second), walk_e(env, std::move(e1)), walk_e(env, std::move(e2)));
		it = env.find(id);
		if (it != env.end())
			return f(v->v, UC(it->second), walk_e(env, std::move(e1)), walk_e(env, std::move(e2)));
	}
	return f(id, std::move(imm), walk_e(env, std::move(e1)), walk_e(env, std::move(e2)));
}

// 各命令の即値最適化
static UExp walk_exp(env_t env, UExp e) {
	// 環境に変数の即値があれば、即値に置き換える
	if(auto e_ = dynamic_cast<Add*>(e.get())) {
		if (auto v = dynamic_cast<V*>(e_->imm.get())) {
			auto it = env.find(v->v);
			if (it != env.end()) return UAdd(e_->id, UC(it->second));
			auto it2 = env.find(e_->id);
			if (it2 == env.end()) return e;
			return UAdd(v->v, UC(it2->second));
		}
	}
	if(auto e_ = dynamic_cast<Sub*>(e.get())) {
		if (auto v = dynamic_cast<V*>(e_->imm.get())) {
			auto it = env.find(v->v);
			if (it == env.end()) return e;
			return USub(e_->id, UC(it->second));
		}
	}
	if(auto e_ = dynamic_cast<Ld*>(e.get())) {
		if (auto v = dynamic_cast<V*>(e_->imm.get())) {
			auto it = env.find(v->v);
			if (it == env.end()) return e;
			return ULd(e_->id, UC(it->second),e_->i);
		}
	}
	if(auto e_ = dynamic_cast<St*>(e.get())) {
		if (auto v = dynamic_cast<V*>(e_->imm.get())) {
			auto it = env.find(v->v);
			if(it==env.end()) return e;
			return USt(e_->id, e_->id2, UC(it->second),e_->i);
		}
	}
	// 分岐命令の場合は両方を変換する
	if(auto e_ = dynamic_cast<IfEq*>(e.get()))
		return walk_exp_if(env, e_->id, std::move(e_->imm), std::move(e_->e1), std::move(e_->e2), UIfEq);
	if(auto e_ = dynamic_cast<IfLE*>(e.get()))
		return walk_exp_if(env, e_->id, std::move(e_->imm), std::move(e_->e1), std::move(e_->e2), UIfLE);
	if(auto e_ = dynamic_cast<IfGE*>(e.get()))
		return walk_exp_if(env, e_->id, std::move(e_->imm), std::move(e_->e1), std::move(e_->e2), UIfGE);
    // 他の命令はそのまま
	return e;
}

// 命令列の即値最適化
static UE walk_e(env_t env, UE e) {
	if(auto e_ = dynamic_cast<Ans*>(e.get()))
		return UAns(walk_exp(env, std::move(e_->exp)));
	if(auto e_ = dynamic_cast<Let*>(e.get())) {
		auto ep = std::move(e);
		// 即値なら
		if (auto e2_ = dynamic_cast<Set*>(e_->exp.get())) {
			auto ep2 = std::move(e_->exp);
			// 環境に保存して後続を計算
			env_t env2 = env;
			env2[e_->id] = e2_->v;
			auto e0 = walk_e(env2, std::move(e_->e));
			// 後続でレジスタを使っていれば、letを残すが、使っていなければ消す
			auto vec = fv(e0.get());
			if (std::find(vec.begin(),vec.end(), e_->id) == vec.end()) return e0;
			return ULet(e_->id, std::move(e_->t), USet(e2_->v), std::move(e0));
		}
		// 即値でなければ、そのまま
		return ULet(e_->id, std::move(e_->t), walk_exp(env, std::move(e_->exp)), walk_e(env, std::move(e_->e)));
	}
	assert(false);
}

// トップレベル関数の即値最適化
static UFundef walk_fundef(UFundef f) {
	return uFundef(f->name, f->args, walk_e(env_t(), std::move(f->body)), std::move(f->ret));
}

// プログラム全体の即値最適化
static UProg walk_prog(UProg prog) {
	auto v = std::vector<UFundef>();
	for (auto& fundef : prog->fundefs)
		v.push_back(walk_fundef(std::move(fundef)));
	return uProg(std::move(v), walk_e(env_t(),std::move(prog->e)));
}

UProg simm(UProg prog) {
	return walk_prog(std::move(prog));
}
