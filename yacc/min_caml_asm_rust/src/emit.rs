use ast::*;
use std::iter::Extend;
use std::collections::HashSet;
use std::error::Error;
use std::io::prelude::*;
use std::fs::File;
use std::path::Path;

fn align(i:i32) -> i32 {
  if i % 8 == 0 {
    i
  } else {
    i + 4
  }
}

fn pp_id_or_imm(imm:&IdOrImm) -> String {
  match *imm {
    IdOrImm::V(ref v) => format!("{}", v),
    IdOrImm::C(ref i) => format!("${}", i),
  }
}

// 関数呼び出しのために引数を並べ替える(register shuffling)
fn shuffle(sw:String , xys:&Vec<(String,String)>) -> Vec<(String, String)> {
  // remove identical moves
  let xys:Vec<(String,String)> = xys.iter().filter(|&&(ref x,ref y)|x!=y).cloned().collect();
  // find acyclic moves
  let xys2 = xys.clone();
  let (xys,mut xys_):(Vec<(String,String)>,Vec<(String,String)>) = xys.into_iter().partition(
    |&(_,ref y)|
      xys2.iter().position(|&(ref a, _)| y.eq(a)) != None
    );
  if xys.len() == 0 && xys_.len() == 0 {
    xys
  } else if xys_.len() != 0 {
    xys_.extend(shuffle(sw, &xys));
    xys_
  } else {
    let (ref x,ref y) = xys[0];
    xys_.push((y.clone(),sw.clone()));
    xys_.push((x.clone(),y.clone()));
    let xys = xys[1..].iter().map(|&(ref y_,ref z)|
      if y.eq(y_) {
        (sw.clone(),z.clone())
      } else {
        (y_.clone(),z.clone())
      }
    ).collect();
    xys_.extend(shuffle(sw, &xys));
    xys_
  }
}
pub struct Emit {
  stackset: HashSet<String>,
  stackmap: Vec<String>,
  count: i32,
  file: File,
}
impl Emit {
  pub fn new(file:&str) -> Emit {
    let path = Path::new(&file);
    let file = match File::create(&path) {
      Err(why) => panic!("couldn't create {}: {}", file, why.description()),
      Ok(file) => file
    };
    Emit{
      stackset:HashSet::new(),
      stackmap:vec![],
      count:0,
      file:file,
    }
  }
  fn print(&mut self,s:String) {
    match self.file.write(s.as_bytes()) {
      Err(why) => panic!("couldn't write to {}: {}", s, why.description()),
      Ok(_) => (),
    }
  }
  fn genid(&mut self, x:&String) -> String {
    self.count += 1;
    format!("{}.{}", x, self.count)
  }
  fn gentmp(&mut self, typ:&T) -> String {
    self.count += 1;
    format!("T{}{}", id_of_typ(typ), self.count)
  }
  fn save(&mut self, x:&String) {
    self.stackset.insert(x.clone());
    if self.stackmap.iter().position(|a| x.eq(a)) == None {
      self.stackmap.push(x.clone());
    }
  }
  fn locate(&mut self, x:&String) -> i32 {
    let r = self.stackmap.iter().position(|a| x.eq(a));
    match r {
      None => -1,
      Some(n) => n as i32,
    }
  }
  fn offset(&mut self, x:&String) -> i32 {
    return 4 * self.locate(x);
  }
  fn stacksize(&mut self) -> i32 {
    return align((self.stackmap.len() * 4) as i32);
  }
  // 命令列のアセンブリ生成
  fn walk_e_tail(&mut self, e:&E) {
    match *e {
      E::Ans(ref exp) => self.walk_exp_tail(exp),
      E::Let(ref id, _,ref exp, ref e) => {
        self.walk_exp_non_tail(id, exp);
        self.walk_e_tail(e);
      } 
    }
  }
  fn walk_e_non_tail(&mut self, x:&String, e:&E) {
    match *e {
      E::Ans(ref exp) => self.walk_exp_non_tail(x, exp),
      E::Let(ref id, _, ref exp, ref e) => {
        self.walk_exp_non_tail(id, exp);
        self.walk_e_non_tail(x, e);
      }
    }
  }
  fn walk_exp_tail_if(&mut self, e1:&E, e2:&E, b:&str, bn:&str) {
    let b_else = self.genid(&format!("{}_else", b));
    self.print(format!("\t{}\t{}\n", bn, b_else));
    let stackset_back = self.stackset.clone();
    self.walk_e_tail(e1);
    self.print(format!("{}:\n", b_else));
    self.stackset = stackset_back;
    self.walk_e_tail(e2);
  }
  fn walk_exp_non_tail_if(&mut self, dest:&String, e1:&E, e2:&E, b:&str, bn:&str) {
    let b_else = self.genid(&format!("{}_else", b));
    let b_cont = self.genid(&format!("{}_cont", b));
    self.print(format!("\t{}\t{}\n", bn, b_else));
    let stackset_back = self.stackset.clone();
    self.walk_e_non_tail(dest, e1);
    let stackset1 = self.stackset.clone();
    self.print(format!("\tjmp\t{}\n", b_cont));
    self.print(format!("{}:\n", b_else));
    self.stackset = stackset_back;
    self.walk_e_non_tail(dest, e2);
    self.print(format!("{}:\n", b_cont));
    self.stackset.extend(stackset1)
  }
  // 末尾だったら計算結果を第一レジスタにセットしてret
  fn walk_exp_tail(&mut self, exp:&Exp) {
    match *exp {
      Exp::Nop | Exp::St(_,_,_,_) | Exp::Save(_,_) => {
        let tmp = self.gentmp(&T::Unit);
        self.walk_exp_non_tail(&tmp, exp);
        self.print(format!("\tret\n"))
      }
      Exp::Set(_) | Exp::SetL(_) | Exp::Mov(_) | Exp::Neg(_) | Exp::Add(_,_) |
      Exp::Sub(_,_) | Exp::Ld(_,_,_) => {
        self.walk_exp_non_tail(&format!("{}",REGS[0]), exp);
        self.print(format!("\tret\n"))
      }
      Exp::Restore(ref id) => {
        assert!(self.locate(id) != -1);
        self.walk_exp_non_tail(&format!("{}",REGS[0]), exp);
        self.print(format!("\tret\n"));
      }
      Exp::IfEq(ref id, ref imm, ref e1, ref e2) => {
        self.print(format!("\tcmpl\t{}, {}\n", pp_id_or_imm(imm), id));
        self.walk_exp_tail_if(e1, e2, "je", "jne");
      }
      Exp::IfLE(ref id, ref imm, ref e1, ref e2) => {
        self.print(format!("\tcmpl\t{}, {}\n", pp_id_or_imm(imm), id));
        self.walk_exp_tail_if(e1, e2, "jle", "jg");
      }
      Exp::IfGE(ref id, ref imm, ref e1, ref e2) => {
        self.print(format!("\tcmpl\t{}, {}\n", pp_id_or_imm(imm), id));
        self.walk_exp_tail_if(e1, e2, "jge", "jl");
      }
      Exp::Call(ref id, ref ids) => {
        // 関数呼び出しの仮想命令の実装
        self.walk_exp_args(vec!(), ids);
        self.print(format!("\tjmp\t{}\n", id));
      }
    }
  }
  fn walk_exp_non_tail(&mut self, dest:&String, exp:&Exp) {
    match *exp {
      Exp::Nop => (),
      Exp::Set(v) => self.print(format!("\tmovl\t${}, {}\n", v, dest)),
      Exp::SetL(ref l) => self.print(format!("\tmovl\t${}, {}\n", l, dest)),
      Exp::Mov(ref id) => if dest != id { self.print(format!("\tmovl\t{}, {}\n", id, dest)) },
      Exp::Neg(ref id) => {
        if dest != id {
          self.print(format!("\tmovl\t{}, {}\n", id, dest))
        }
        self.print(format!("\tnegl\t{}\n", dest))
      }
      Exp::Add(ref id, IdOrImm::V(ref v)) if dest.eq(v) =>
        self.print(format!("\taddl\t{}, {}\n", id, dest)),
      Exp::Add(ref id, ref imm) => {
        if dest != id {
          self.print(format!("\tmovl\t{}, {}\n", id, dest))
        }
        self.print(format!("\taddl\t{}, {}\n", pp_id_or_imm(imm), dest))
      }
      Exp::Sub(ref id, IdOrImm::V(ref v)) if dest.eq(v) => {
        self.print(format!("\tsubl\t{}, {}\n", id, dest));
        self.print(format!("\tnegl\t{}\n", dest))
      }
      Exp::Sub(ref id, ref imm) => {
        if dest != id {
          self.print(format!("\tmovl\t{}, {}\n", id, dest))
        }
        self.print(format!("\tsubl\t{}, {}\n", pp_id_or_imm(imm), dest))
      }
      Exp::Ld(ref id, IdOrImm::V(ref v), i) =>
        self.print(format!("\tmovl\t({},{},{}), {}\n", id, v, i, dest)),
      Exp::Ld(ref id, IdOrImm::C(ref j), i) =>
        self.print(format!("\tmovl\t{}({}), {}\n", j * i, id, dest)),
      Exp::St(ref id, ref id2, IdOrImm::V(ref v), i) =>
        self.print(format!("\tmovl\t{}, ({},{},{})\n", id, id2, v, i)),
      Exp::St(ref id, ref id2, IdOrImm::C(ref j), i) =>
        self.print(format!("\tmovl\t{}, {}({})\n", id, j * i, id2)),
      // 退避の仮想命令の実装
      Exp::Save(ref id, ref id2) => {
        if allregs_find(id) && !self.stackset.contains(id2) {
          self.save(id2);
          let offset = self.offset(id2);
          self.print(format!("\tmovl\t{}, {}({})\n", id, offset, REG_SP))
        } else {
          assert!(self.stackset.contains(id2))
        }
      }
      // 復帰の仮想命令の実装
      Exp::Restore(ref id) => {
        let offset = self.offset(id);
        self.print(format!("\tmovl\t{}({}), {}\n", offset, REG_SP, dest))
      }
      Exp::IfEq(ref id, ref imm, ref e1, ref e2) => {
        self.print(format!("\tcmpl\t{}, {}\n", pp_id_or_imm(imm), id));
        self.walk_exp_non_tail_if(dest, e1, e2, "je", "jne");
      }
      Exp::IfLE(ref id, ref imm, ref e1, ref e2) => {
        self.print(format!("\tcmpl\t{}, {}\n", pp_id_or_imm(imm), id));
        self.walk_exp_non_tail_if(dest, e1, e2, "jle", "jg");
      }
      Exp::IfGE(ref id, ref imm, ref e1, ref e2) => {
        self.print(format!("\tcmpl\t{}, {}\n", pp_id_or_imm(imm), id));
        self.walk_exp_non_tail_if(dest, e1, e2, "jge", "jl");
      }
      Exp::Call(ref id, ref ids) => {
        self.walk_exp_args(vec!(), ids);
        let ss = self.stacksize();
        if ss > 0 {
          self.print(format!("\taddl\t${}, {}\n", ss, REG_SP))
        }
        self.print(format!("\tcall\t{}\n", id));
        if ss > 0 {
          self.print(format!("\tsubl\t${}, {}\n", ss, REG_SP))
        }
        if allregs_find(dest) && dest != REGS[0] {
          self.print(format!("\tmovl\t{}, {}\n", REGS[0], dest))
        }
      }
    }
  }
  fn walk_exp_args(&mut self, mut yrs:Vec<(String,String)>, ys:&Vec<String>) {
    assert!(ys.len() <= REGS.len() - yrs.len());
    let sw = format!("{}({})", self.stacksize(), REG_SP);
    let mut i = 0;
    for y in ys.iter() {
      yrs.push((y.clone(), format!("{}",REGS[i])));
      i+=1;
    }
    let yrs2 = shuffle(sw, &yrs);
    for (y,r) in yrs2 {
      self.print(format!("\tmovl\t{}, {}\n", y, r));
    }
  }
  fn walk_fundef(&mut self, fundef:&Fundef) {
    match *fundef {
      Fundef::Fundef(ref name, _, ref body, _) => {
        self.print(format!("{}:\n", name));
        self.stackset.clear();
        self.stackmap.clear();
        self.walk_e_tail(body);
      }
    }
  }
  fn walk_prog(&mut self, prog:&Prog) {
    match *prog {
      Prog::Prog(ref fundefs,ref e) => {
        //fprintf(stderr, "generating assembly...\n");
        self.print(format!(".text\n"));
        for fundef in fundefs {
          self.walk_fundef(fundef)
        }
        self.print(format!(".globl\tmin_caml_start\n"));
        self.print(format!("min_caml_start:\n"));
        self.print(format!(".globl\t_min_caml_start\n"));
        self.print(format!("_min_caml_start: # for cygwin\n"));
        self.print(format!("\tpushl\t%eax\n"));
        self.print(format!("\tpushl\t%ebx\n"));
        self.print(format!("\tpushl\t%ecx\n"));
        self.print(format!("\tpushl\t%edx\n"));
        self.print(format!("\tpushl\t%esi\n"));
        self.print(format!("\tpushl\t%edi\n"));
        self.print(format!("\tpushl\t%ebp\n"));
        self.print(format!("\tmovl\t32(%esp),{}\n", REG_SP));
        self.print(format!("\tmovl\t36(%esp),{}\n", REGS[0]));
        self.print(format!("\tmovl\t{},{}\n", REGS[0], REG_HP));
        self.stackset.clear();
        self.stackmap.clear();
        self.walk_e_non_tail(&format!("{}",REGS[0]), e);
        self.print(format!("\tpopl\t%ebp\n"));
        self.print(format!("\tpopl\t%edi\n"));
        self.print(format!("\tpopl\t%esi\n"));
        self.print(format!("\tpopl\t%edx\n"));
        self.print(format!("\tpopl\t%ecx\n"));
        self.print(format!("\tpopl\t%ebx\n"));
        self.print(format!("\tpopl\t%eax\n"));
        self.print(format!("\tret\n"));
      }
    }
  }
}
pub fn emit(file:&String, prog:&Prog) {
  Emit::new(file).walk_prog(prog)
}
