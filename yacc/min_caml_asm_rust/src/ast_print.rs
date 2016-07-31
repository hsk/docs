use ast::*;

pub trait P {
  fn p(&self) -> String;
}

impl P for T {
  fn p(&self) -> String {
    match *self {
      T::Unit => "unit".to_string(),
      T::Bool => "bool".to_string(),
      T::Int => "int".to_string(),
      T::Fun(ref ls, ref t) => {
        let mut stack = String::new();
        for l in ls {
          stack = stack + &l.p();
          stack.push_str(", ");
        }
        let x: &[_] = &[',', ' '];
        format!("({})->{}", stack.trim_right_matches(x), t.p())
      }
    }
  }
}

impl P for IdOrImm {
  fn p(&self) -> String {
    match *self {
      IdOrImm::V(ref s) => s.clone(),
      IdOrImm::C(ref i) => format!("{}",i),
    }
  }
}
pub trait P2 {
  fn p2(&self,sp:String) -> String;
}
impl P for Exp {
  fn p(&self) -> String {
    self.p2(format!(""))
  }
}
impl P for E {
  fn p(&self) -> String {
    self.p2(format!(""))
  }
}
impl P2 for Exp {
  fn p2(&self, sp:String) -> String {
    let sp2 = format!("  {}", sp);
    match *self {
      Exp::Nop => format!("nop\n"),
      Exp::Set(i) => format!("set {}\n", i),
      Exp::SetL(ref id) => format!("setl {}\n",id),
      Exp::Mov(ref id) => format!("mov {}\n", id),
      Exp::Neg(ref id) => format!("neg {}\n", id),
      Exp::Add(ref id, ref imm) => format!("add {} {}\n", id, imm.p()),
      Exp::Sub(ref id, ref imm) => format!("sub {} {}\n", id, imm.p()),
      Exp::Ld(ref id, ref imm,i) => format!("ld {} {} {}\n", id, imm.p(), i),
      Exp::St(ref id, ref id2, ref imm,i) => format!("st {} {} {} {}\n", id, id2, imm.p(), i),
      Exp::IfEq(ref id, ref imm, ref e1, ref e2) =>
        format!("ifeq {} {} {{\n{}{}}} else {{\n{}{}}}\n", id, imm.p(), e1.p2(sp2.clone()), sp, e2.p2(sp2), sp),
      Exp::IfLE(ref id, ref imm, ref e1, ref e2) =>
        format!("ifeq {} {} {{\n{}{}}} else {{\n{}{}}}\n", id, imm.p(), e1.p2(sp2.clone()), sp, e2.p2(sp2), sp),
      Exp::IfGE(ref id, ref imm, ref e1, ref e2) =>
        format!("ifeq {} {} {{\n{}{}}} else {{\n{}{}}}\n", id, imm.p(), e1.p2(sp2.clone()), sp, e2.p2(sp2), sp),
      Exp::Call(ref id, ref ids) => format!("call {} ({})\n", id, ids.join(", ")),
      Exp::Save(ref id, ref id2) => format!("save {} {}\n", id, id2),
      Exp::Restore(ref id) => format!("restore {}\n", id),
    }
  }
}
impl P2 for E {
  fn p2(&self, sp:String) -> String {
    match *self {
      E::Ans(ref exp) => format!("{}{}", sp.clone(), exp.p2(sp)),
      E::Let(_, T::Unit, ref exp, ref e) => format!("{}{}{}", sp.clone(), exp.p2(sp.clone()), e.p2(sp)),
      E::Let(ref id, ref t, ref exp, ref e) => format!("{}{} : {} = {}{}", sp.clone(), id, t.p(), exp.p2(sp.clone()), e.p2(sp)),
    }
  }
}
impl P for Fundef {
  fn p(&self) -> String {
    match *self {
      Fundef::Fundef(ref id, ref ids, ref e, ref t) => {
        let ids2 = ids.iter().map(|x| format!("{}:int",x)).collect::<Vec<_>>();
        format!("define {} ({}): {} {{\n{}}}\n", id, ids2.join(", "), t.p(), e.p2(format!("  ")))
      }
    }
  }
}
impl P for Prog {
  fn p(&self) -> String {
    match *self {
      Prog::Prog(ref fundefs, ref e) => {
        let fundefs = fundefs.iter().map(|x| x.p()).collect::<Vec<_>>();
        format!("{}{}", fundefs.join(""), e.p())
      }
    }
  }
}

