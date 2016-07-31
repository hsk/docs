pub const REG_SP : &'static str = "%ebp";
pub const REG_HP : &'static str = "min_caml_hp";
pub const REGS: &'static [&'static str] = &["%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi"];
pub const REG_CL : &'static str = "%edi";
pub fn allregs_find(s:&String) -> bool {
  REGS.contains(&s.as_str())
}

pub fn id_of_typ(t:&T) -> String {
  match *t {
    T::Unit => format!("u"),
    T::Bool => format!("b"),
    T::Int => format!("i"),
    T::Fun(_,_) => format!("f"),
  }
}

#[derive(Clone,Debug,PartialEq)]
pub enum T {
  Unit,
  Bool,
  Int,
  Fun(Vec<T>,Box<T>),
}
#[derive(Clone,Debug,PartialEq)]
pub enum IdOrImm {
  V(String),
  C(i32),
}
#[derive(Clone,Debug,PartialEq)]
pub enum Exp {
  Nop,
  Set(i32),
  SetL(String),
  Mov(String),
  Neg(String),
  Add(String,IdOrImm),
  Sub(String,IdOrImm),
  Ld(String,IdOrImm,i32),
  St(String,String,IdOrImm,i32),
  IfEq(String,IdOrImm,Box<E>,Box<E>),
  IfLE(String,IdOrImm,Box<E>,Box<E>),
  IfGE(String,IdOrImm,Box<E>,Box<E>),
  Call(String,Vec<String>),
  Save(String,String),
  Restore(String),
}
#[derive(Clone,Debug,PartialEq)]
pub enum E {
  Ans(Exp),
  Let(String,T,Exp,Box<E>),
}
#[derive(Clone,Debug,PartialEq)]
pub enum Fundef {
  Fundef(String,Vec<String>,E,T)
}  
#[derive(Clone,Debug,PartialEq)]
pub enum Prog {
  Prog(Vec<Fundef>,E)
}

pub fn fv(e:&E) -> Vec<String> {
  use std::collections::HashSet;
  fn remove_and_uniq(mut xs: HashSet<String>, ls:Vec<String>) -> Vec<String> {
    ls.iter().filter(|&l|
      if !xs.contains(l) {
        xs.insert(l.clone());
        true
      } else {
        false
      }
    ).cloned().collect()
  }
  fn fv_id_or_imm(imm:&IdOrImm) -> Vec<String> {
    match *imm {
      IdOrImm::V(ref v) => vec![v.clone()],
      IdOrImm::C(_) => vec![],
    }
  }
  fn fv_exp(e:&Exp) -> Vec<String> {
    match *e {
      Exp::Nop | Exp::Set(_) | Exp::SetL(_) | Exp::Restore(_) => vec!(),
      Exp::Mov(ref id) | Exp::Neg(ref id) | Exp::Save(ref id, _) => vec!{id.clone()},
      Exp::Add(ref id, ref imm) |  Exp::Sub(ref id, ref imm) | 
      Exp::Ld(ref id, ref imm, _) => {
        let mut v = vec!{id.clone()};
        v.extend(fv_id_or_imm(imm));
        v
      }
      Exp::St(ref id, ref id2, ref imm, _) => {
        let mut v = vec!{id.clone(),id2.clone()};
        v.extend(fv_id_or_imm(imm));
        v
      }
      Exp::Call(_, ref ids) => ids.clone(),
      Exp::IfEq(ref id, ref imm, ref e1, ref e2) |
      Exp::IfLE(ref id, ref imm, ref e1, ref e2) |
      Exp::IfGE(ref id, ref imm, ref e1, ref e2) => {
        let mut v = vec!{id.clone()};
        v.extend(fv_id_or_imm(imm));
        let mut v2 = fv(e1);
        v2.extend(fv(e2));
        v.extend(remove_and_uniq(HashSet::new(), v2));
        v
      }
    }
  }
  fn fv_e(e:&E) -> Vec<String> {
    match *e {
      E::Ans(ref exp) => fv_exp(exp),
      E::Let(ref id, _, ref exp, ref e1) => {
        let mut v = fv_exp(exp);
        let mut set = HashSet::new();
        set.insert(id.clone());
        v.extend(remove_and_uniq(set, fv_e(&e1)));
        v
      }
    }
  }

  remove_and_uniq(HashSet::new(), fv_e(e))
}

use std::cell::Cell;
thread_local!(static F: Cell<i32> = Cell::new(0));
fn gencnt() -> i32 {
  F.with(|f| {
    let cnt = f.get();
    f.set(cnt+1);
    cnt
  })
}

pub fn genid(s:&str) -> String {
  format!("{}{}",s, gencnt())
}

pub fn gentmp(t:&T) -> String {
    genid(&*id_of_typ(t))
}

pub fn seq(e1:Exp, e2:E) -> E {
  E::Let(gentmp(&T::Unit), T::Unit, e1, Box::new(e2))
}
