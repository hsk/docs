#include "ast.h"
#include <cassert>
#include <algorithm>
// すでにSaveされた変数の集合 (caml2html: emit_stackset)

static
std::set<std::string> stackset;

// Saveされた変数の、スタックにおける位置 (caml2html: emit_stackmap) *)
static
svec_t stackmap;

static
void save(std::string x) {
	stackset.insert(x);
	svec_t stackmap;
	auto it = std::find(stackmap.begin(), stackmap.end(), x);
	if (it == stackmap.end()) stackmap.push_back(x);
}

static
int locate(std::string x) {
	auto iter = std::find(stackmap.begin(), stackmap.end(), x);
	size_t index = std::distance(stackmap.begin(), iter);
	if (index == stackmap.size()) return -1;
	return index;
}

static
int offset(std::string x) {
	return 4 * locate(x);
}

static
int stacksize() {
	return align(stackmap.size() * 4);
}

static
std::string pp_id_or_imm(Id_or_imm* imm) {
	if (auto im = dynamic_cast<V*> (imm)) return im->v;
	if (auto im = dynamic_cast<C*> (imm))
		return std::string("$") + std::to_string(im->i);
	return std::string("");
}

// 関数呼び出しのために引数を並べ替える(register shuffling) (caml2html: emit_shuffle)

static
std::vector<std::pair<std::string, std::string>>
shuffle(std::string sw, std::vector<std::pair<std::string, std::string>> xys) {
	std::map<std::string, std::string> xys2;
	// remove identical moves
	for (auto xy : xys) {
		if (xy.first == xy.second) continue;
		xys2[xy.first] = xy.second;
	}

	// find acyclic moves
	std::vector<std::pair<std::string, std::string>> xys3;
	std::vector<std::pair<std::string, std::string>> xys4;
	for (auto xy : xys2) {
		auto it = xys2.find(xy.second);
		if (it != xys2.end()) xys3.push_back(xy);
		else xys4.push_back(xy);
	}
	if (xys3.empty() && xys4.empty()) return xys3;
	if (xys4.empty()) {
		auto xy = xys3[0];
		xys3.erase(xys3.begin());
		xys4.push_back(std::pair<std::string, std::string>(xy.second, sw));
		xys4.push_back(xy);
		std::vector<std::pair<std::string, std::string>> xys5;
		for (auto xy2 : xys3) {
			if (xy.second == xy2.first)
				xys5.push_back(std::pair<std::string, std::string>(sw, xy2.second));
			else xys5.push_back(xy2);
		}
		return insert<std::pair<std::string, std::string>>(xys4,shuffle(sw, xys5));
	}
	return insert<std::pair<std::string, std::string>>(xys4,shuffle(sw, xys3));
}

/*
type dest =
  | Tail
  | NonTail of Id.t (* 末尾かどうかを表すデータ型 (caml2html: emit_dest) *)
 */

static
void walk_exp_tail(FILE* oc, Exp* exp);
static
void walk_exp_non_tail(FILE* oc, std::string x, Exp* exp);

// 命令列のアセンブリ生成 (caml2html: emit_g)

static
void walk_e_tail(FILE* oc, E* e) {
	if (auto e_ = dynamic_cast<Ans*> (e))
		return walk_exp_tail(oc, e_->exp.get());
	if (auto e_ = dynamic_cast<Let*> (e)) {
		walk_exp_non_tail(oc, e_->id, e_->exp.get());
		walk_e_tail(oc, e_->e.get());
	}
}

static
void walk_e_non_tail(FILE* oc, std::string x, E* e) {
	if (auto e_ = dynamic_cast<Ans*> (e))
		return walk_exp_non_tail(oc, x, e_->exp.get());
	if (auto e_ = dynamic_cast<Let*> (e)) {
		walk_exp_non_tail(oc, e_->id, e_->exp.get());
		walk_e_non_tail(oc, x, e_->e.get());
	}
}

static
void walk_exp_tail_if(FILE* oc, E* e1, E* e2, std::string b, std::string bn) {
	auto b_else = genid(b + "_else");
	fprintf(oc, "\t%s\t%s\n", bn.c_str(), b_else.c_str());
	auto stackset_back = stackset;
	walk_e_tail(oc, e1);
	fprintf(oc, "%s:\n", b_else.c_str());
	stackset = stackset_back;
	walk_e_tail(oc, e2);
}

static
void walk_exp_non_tail_if(FILE* oc, std::string dest, E* e1, E* e2, std::string b, std::string bn) {
	auto b_else = genid(b + "_else");
	auto b_cont = genid(b + "_cont");
	fprintf(oc, "\t%s\t%s\n", bn.c_str(), b_else.c_str());
	auto stackset_back = stackset;
	walk_e_non_tail(oc, dest, e1);
	auto stackset1 = stackset;
	fprintf(oc, "\tjmp\t%s\n", b_cont.c_str());
	fprintf(oc, "%s:\n", b_else.c_str());
	stackset = stackset_back;
	walk_e_non_tail(oc, dest, e2);
	fprintf(oc, "%s:\n", b_cont.c_str());
	stackset.insert(stackset1.begin(), stackset1.end());
}

static
void walk_exp_args(FILE* oc, std::vector<std::pair<std::string, std::string>>, svec_t);

// 各命令のアセンブリ生成 (caml2html: emit_gprime)

static
void walk_exp_tail(FILE* oc, Exp* exp) {
	// 末尾だったら計算結果を第一レジスタにセットしてret (caml2html: emit_tailret)
	if (dynamic_cast<Nop*> (exp) || dynamic_cast<St*> (exp) || dynamic_cast<Save*> (exp)) {
		auto unit = std::unique_ptr<T>(new Unit());
		walk_exp_non_tail(oc, gentmp(unit.get()), exp);
		fprintf(oc, "\tret\n");
	} else
	if (dynamic_cast<Set*> (exp) || dynamic_cast<SetL*> (exp) || dynamic_cast<Mov*> (exp)
			|| dynamic_cast<Neg*> (exp) || dynamic_cast<Add*> (exp) || dynamic_cast<Sub*> (exp)
			|| dynamic_cast<Ld*> (exp)) {
		walk_exp_non_tail(oc, regs[0], exp);
		fprintf(oc, "\tret\n");
	} else
	if (auto e_ = dynamic_cast<Restore*> (exp)) {
		assert(locate(e_->id) != -1);
		walk_exp_non_tail(oc, regs[0], exp);
		fprintf(oc, "\tret\n");
	} else
	if (auto e_ = dynamic_cast<IfEq*> (exp)) {
		fprintf(oc, "\tcmpl\t%s, %s\n", pp_id_or_imm(e_->imm.get()).c_str(), e_->id.c_str());
		walk_exp_tail_if(oc, e_->e1.get(), e_->e2.get(), "je", "jne");
	} else
	if (auto e_ = dynamic_cast<IfLE*> (exp)) {
		fprintf(oc, "\tcmpl\t%s, %s\n", pp_id_or_imm(e_->imm.get()).c_str(), e_->id.c_str());
		walk_exp_tail_if(oc, e_->e1.get(), e_->e2.get(), "jle", "jg");
	} else
	if (auto e_ = dynamic_cast<IfGE*> (exp)) {
		fprintf(oc, "\tcmpl\t%s, %s\n", pp_id_or_imm(e_->imm.get()).c_str(), e_->id.c_str());
		walk_exp_tail_if(oc, e_->e1.get(), e_->e2.get(), "jge", "jl");
	} else
	if (auto e_ = dynamic_cast<Call*> (exp)) {
		// 関数呼び出しの仮想命令の実装 (caml2html: emit_call)
		walk_exp_args(oc, std::vector<std::pair < std::string, std::string >> (), e_->ids);
		fprintf(oc, "\tjmp\t%s\n", e_->id.c_str());
	}
}

static
void walk_exp_non_tail(FILE* oc, std::string dest, Exp* exp) {
	// 末尾でなかったら計算結果をdestにセット (caml2html: emit_nontail)
	if (dynamic_cast<Nop*> (exp)) {
	} else
	if (auto e_ = dynamic_cast<Set*> (exp)) {
		fprintf(oc, "\tmovl\t$%d, %s\n", e_->v, dest.c_str());
	} else
	if (auto e_ = dynamic_cast<SetL*> (exp)) {
		fprintf(oc, "\tmovl\t$%s, %s\n", e_->l.c_str(), dest.c_str());
	} else
	if (auto e_ = dynamic_cast<Mov*> (exp)) {
		if (dest != e_->id)
			fprintf(oc, "\tmovl\t%s, %s\n", e_->id.c_str(), dest.c_str());
	} else
	if (auto e_ = dynamic_cast<Neg*> (exp)) {
		if (dest != e_->id)
			fprintf(oc, "\tmovl\t%s, %s\n", e_->id.c_str(), dest.c_str());
		fprintf(oc, "\tnegl\t%s\n", dest.c_str());
	} else
	if (auto e_ = dynamic_cast<Add*> (exp)) {
		if (auto v = dynamic_cast<V*> (e_->imm.get())) {
			if (v->v == dest) {
				fprintf(oc, "\taddl\t%s, %s\n", e_->id.c_str(), dest.c_str());
				return;
			}
		}
		if (dest != e_->id)
			fprintf(oc, "\tmovl\t%s, %s\n", e_->id.c_str(), dest.c_str());
		fprintf(oc, "\taddl\t%s, %s\n", pp_id_or_imm(e_->imm.get()).c_str(), dest.c_str());
	} else
	if (auto e_ = dynamic_cast<Sub*> (exp)) {
		if (auto v = dynamic_cast<V*> (e_->imm.get())) {
			if (v->v == dest) {
				fprintf(oc, "\tsubl\t%s, %s\n", e_->id.c_str(), dest.c_str());
				fprintf(oc, "\tnegl\t%s\n", dest.c_str());
				return;
			}
		}
		if (dest != e_->id)
			fprintf(oc, "\tmovl\t%s, %s\n", e_->id.c_str(), dest.c_str());
		fprintf(oc, "\tsubl\t%s, %s\n", pp_id_or_imm(e_->imm.get()).c_str(), dest.c_str());
	} else
	if (auto e_ = dynamic_cast<Ld*> (exp)) {
		if (auto v = dynamic_cast<V*> (e_->imm.get())) {
			fprintf(oc, "\tmovl\t(%s,%s,%d), %s\n", e_->id.c_str(), v->v.c_str(), e_->i, dest.c_str());
			return;
		}
		if (auto c = dynamic_cast<C*> (e_->imm.get())) {
			fprintf(oc, "\tmovl\t%d(%s), %s\n", c->i * e_->i, e_->id.c_str(), dest.c_str());
			return;
		}
		assert(false);
	} else
	if (auto e_ = dynamic_cast<St*> (exp)) {
		if (auto v = dynamic_cast<V*> (e_->imm.get())) {
			fprintf(oc, "\tmovl\t%s, (%s,%s,%d)\n", e_->id.c_str(), e_->id2.c_str(), v->v.c_str(), e_->i);
			return;
		}
		if (auto c = dynamic_cast<C*> (e_->imm.get())) {
			fprintf(oc, "\tmovl\t%s, %d(%s)\n", e_->id.c_str(), c->i * e_->i, e_->id2.c_str());
			return;
		}
		assert(false);
	} else
	// 退避の仮想命令の実装 (caml2html: emit_save)
	if (auto e_ = dynamic_cast<Save*> (exp)) {
		if (allregs.find(e_->id) != allregs.end() && stackset.find(e_->id2) == stackset.end()) {
			save(e_->id2);
			fprintf(oc, "\tmovl\t%s, %d(%s)\n", e_->id.c_str(), offset(e_->id2), reg_sp);
			return;
		}
		assert(stackset.find(e_->id2) != stackset.end());
	} else
	if (auto e_ = dynamic_cast<Restore*> (exp)) {
		// 復帰の仮想命令の実装 (caml2html: emit_restore)
		fprintf(oc, "\tmovl\t%d(%s), %s\n", offset(e_->id), reg_sp, dest.c_str());
	} else
	if (auto e_ = dynamic_cast<IfEq*> (exp)) {
		fprintf(oc, "\tcmpl\t%s, %s\n", pp_id_or_imm(e_->imm.get()).c_str(), e_->id.c_str());
		walk_exp_non_tail_if(oc, dest, e_->e1.get(), e_->e2.get(), "je", "jne");
	} else
	if (auto e_ = dynamic_cast<IfLE*> (exp)) {
		fprintf(oc, "\tcmpl\t%s, %s\n", pp_id_or_imm(e_->imm.get()).c_str(), e_->id.c_str());
		walk_exp_non_tail_if(oc, dest, e_->e1.get(), e_->e2.get(), "jle", "jg");
	} else
	if (auto e_ = dynamic_cast<IfGE*> (exp)) {
		fprintf(oc, "\tcmpl\t%s, %s\n", pp_id_or_imm(e_->imm.get()).c_str(), e_->id.c_str());
		walk_exp_non_tail_if(oc, dest, e_->e1.get(), e_->e2.get(), "jge", "jl");
	} else
	if (auto e_ = dynamic_cast<Call*> (exp)) {
		walk_exp_args(oc, std::vector<std::pair < std::string, std::string >> (), e_->ids);
		auto ss = stacksize();
		if (ss > 0) fprintf(oc, "\taddl\t$%d, %s\n", ss, reg_sp);
		fprintf(oc, "\tcall\t%s\n", e_->id.c_str());
		if (ss > 0) fprintf(oc, "\tsubl\t$%d, %s\n", ss, reg_sp);
		if (allregs.find(dest) != allregs.end() && dest != regs[0]) {
			fprintf(oc, "\tmovl\t%s, %s\n", regs[0].c_str(), dest.c_str());
		}
	} else {
		assert(false);
	}
}

static
void walk_exp_args(FILE* oc, std::vector<std::pair<std::string, std::string>> yrs, svec_t ys) {
	assert(ys.size() <= regs.size() - yrs.size());
	auto sw = std::string() + std::to_string(stacksize()) + ("(" reg_sp ")");
	int i = 0;
	for (auto y : ys) {
		yrs.push_back(std::pair<std::string, std::string>(y, regs[i]));
		i++;
	}
	auto yrs2 = shuffle(sw, yrs);
	for (auto yr : yrs2)
		fprintf(oc, "\tmovl\t%s, %s\n", yr.first.c_str(), yr.second.c_str());
}

static
void walk_fundef(FILE* oc, Fundef* fundef) {
	fprintf(oc, "%s:\n", fundef->name.c_str());
	stackset.clear();
	stackmap.clear();
	walk_e_tail(oc, fundef->body.get());
}

static
void walk_prog(FILE* oc, Prog* prog) {
	fprintf(stderr, "generating assembly...\n");
	fprintf(oc, ".text\n");
	for (auto& fundef : prog->fundefs)
		walk_fundef(oc, fundef.get());
	fprintf(oc, ".globl\tmin_caml_start\n");
	fprintf(oc, "min_caml_start:\n");
	fprintf(oc, ".globl\t_min_caml_start\n");
	fprintf(oc, "_min_caml_start: # for cygwin\n");
	fprintf(oc, "\tpushl\t%%eax\n");
	fprintf(oc, "\tpushl\t%%ebx\n");
	fprintf(oc, "\tpushl\t%%ecx\n");
	fprintf(oc, "\tpushl\t%%edx\n");
	fprintf(oc, "\tpushl\t%%esi\n");
	fprintf(oc, "\tpushl\t%%edi\n");
	fprintf(oc, "\tpushl\t%%ebp\n");
	fprintf(oc, "\tmovl\t32(%%esp),%s\n", reg_sp);
	fprintf(oc, "\tmovl\t36(%%esp),%s\n", regs[0].c_str());
	fprintf(oc, "\tmovl\t%s,%s\n", regs[0].c_str(), reg_hp);
	stackset.clear();
	stackmap.clear();
	walk_e_non_tail(oc, regs[0], prog->e.get());
	fprintf(oc, "\tpopl\t%%ebp\n");
	fprintf(oc, "\tpopl\t%%edi\n");
	fprintf(oc, "\tpopl\t%%esi\n");
	fprintf(oc, "\tpopl\t%%edx\n");
	fprintf(oc, "\tpopl\t%%ecx\n");
	fprintf(oc, "\tpopl\t%%ebx\n");
	fprintf(oc, "\tpopl\t%%eax\n");
	fprintf(oc, "\tret\n");
}

void emit(FILE* oc, Prog* prog) {
	walk_prog(oc, prog);
}
