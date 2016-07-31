#include "ast.h"
#include <cassert>

std::string concat(std::string str, svec_t vec) {
	std::string r ="";
	for (auto s : vec) {
		if(!r.empty()) r += str;
		r += s;
	}
	return r;
}

static svec_t map(svec_t vec, std::function<std::string(std::string)> f) {
	svec_t r;
	for (auto s : vec) r.push_back(f(s));
	return r;
}

std::string show_type(T* t);
std::string show_types(std::vector<std::unique_ptr<T>>& ts) {
	svec_t r;
	for(auto& t : ts) r.push_back(show_type(t.get()));
	return concat(", ", r);
}

std::string show_type(T* t) {
	if (dynamic_cast<Unit*>(t)) return "unit";
	if (dynamic_cast<Bool*>(t)) return "bool";
	if (dynamic_cast<Int*>(t)) return "int";
	if (auto t_ = dynamic_cast<Fun*>(t)) return "(" + show_types(t_->ts) + ") -> " + show_type(t_->t.get());	
	assert(false);
}

std::string show_imm(Id_or_imm* imm) {
	if(auto im = dynamic_cast<V*>(imm)) return im->v;
	if(auto im = dynamic_cast<C*>(imm)) return std::to_string(im->i);
	assert(false);
}

std::string show_e(std::string sp, E* e);

std::string show_exp(std::string sp, Exp* e) {
	if(auto e_ = dynamic_cast<Nop*>(e)) return "nop\n";
	if(auto e_ = dynamic_cast<Set*>(e)) return "set "+std::to_string(e_->v)+"\n";
	if(auto e_ = dynamic_cast<SetL*>(e)) return "setl "+e_->l+"\n";
	if(auto e_ = dynamic_cast<Mov*>(e)) return "mov "+e_->id+"\n";
	if(auto e_ = dynamic_cast<Neg*>(e)) return "neg "+e_->id+"\n";
	if(auto e_ = dynamic_cast<Add*>(e)) return "add "+e_->id+" "+show_imm(e_->imm.get())+"\n";
	if(auto e_ = dynamic_cast<Sub*>(e)) return "sub "+e_->id+" "+show_imm(e_->imm.get())+"\n";
	if(auto e_ = dynamic_cast<Ld*>(e)) return "ld "+e_->id+" "+show_imm(e_->imm.get())+" "+std::to_string(e_->i)+"\n";
	if(auto e_ = dynamic_cast<St*>(e)) return "st "+e_->id+" "+e_->id2+" "+show_imm(e_->imm.get())+" "+std::to_string(e_->i)+"\n";
	/*
  | Comment(id)-> Printf.printf "comment %s\n" id*/
	if(auto e_ = dynamic_cast<IfEq*>(e))
		return "ifeq "+e_->id+" "+show_imm(e_->imm.get())+" {\n"+
				show_e(sp + "  ", e_->e1.get()) + sp + "} else {\n" +
				show_e(sp + "  ", e_->e2.get()) + sp + "}\n";
	if(auto e_ = dynamic_cast<IfLE*>(e))
		return "ifle "+e_->id+" "+show_imm(e_->imm.get())+" {\n"+
				show_e(sp + "  ", e_->e1.get()) + sp + "} else {\n" +
				show_e(sp + "  ", e_->e2.get()) + sp + "}\n";
	if(auto e_ = dynamic_cast<IfGE*>(e))
		return "ifge "+e_->id+" "+show_imm(e_->imm.get())+" {\n"+
				show_e(sp + "  ", e_->e1.get()) + sp + "} else {\n" +
				show_e(sp + "  ", e_->e2.get()) + sp + "}\n";
	/*
  | CallCls(id, ids) ->
  	Printf.printf "cls %s(%s)\n" id
  		(String.concat "," (ids|>List.map(fun s->s^":int")))*/
	if(auto e_ = dynamic_cast<Call*>(e)) return "call "+e_->id+"("+concat(", ",map(e_->ids,[](std::string s){return s+":int";}))+")\n";
	if(auto e_ = dynamic_cast<Save*>(e)) return "save "+e_->id+" "+e_->id2+"\n";
	if(auto e_ = dynamic_cast<Restore*>(e)) return "restore "+e_->id+"\n";
	assert(false);
}

std::string show_e(std::string sp, E* e) {
	if(auto e_ = dynamic_cast<Ans*>(e))
		return sp + show_exp(sp, e_->exp.get());
	if(auto e_ = dynamic_cast<Let*>(e)) {
		if(dynamic_cast<Unit*>(e_->t.get()))
			return sp + show_exp(sp, e_->exp.get()) + show_e(sp, e_->e.get());
		return sp + e_->id + " : " + show_type(e_->t.get()) + " = " + show_exp(sp, e_->exp.get()) + show_e(sp,e_->e.get());
	}
	assert(false);
}

std::string show_fundef(Fundef* fundef) {
	return "define "+fundef->name+
			" ("+concat(", ",map(fundef->args,[](std::string s){return s+":int";}))+ "):"+
			show_type(fundef->ret.get())+" {\n" +
			show_e("  ", fundef->body.get())+ "}\n";
}

std::string show_prog (Prog* prog) {
	std::string str;
	for(auto& fundef : prog->fundefs)
		str += show_fundef(fundef.get());
	return str + show_e("", prog->e.get());
}

std::string show_regenv(regenv_t revenv) {
	svec_t r;
	for (auto it : revenv) r.push_back(it.first + ":" + it.second);
	return concat(", ", r);
}
