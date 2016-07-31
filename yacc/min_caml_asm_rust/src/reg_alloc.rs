use ast::*;
use std::collections::HashMap;
use std::collections::HashSet;

// auxiliary function for Call
fn target_args(src:&String, all:&'static [&'static str], args:&Vec<String>) -> Vec<String> {
  let mut rc = vec!();
  let mut n = 0;
  for y in args.iter() {
    if *src == *y {
      rc.push(format!("{}",all[n]))
    }
    n += 1;
  }
  rc
}

fn is_reg(x:&String) -> bool {
  x.starts_with('%') || x == REG_HP
}

// for register coalescing
// [XXX] Callがあったら、そこから先は無意味というか逆効果なので追わない。
//         そのために「Callがあったかどうか」を返り値の第1要素に含める。
fn target_exp(src:&String, dest:&(&String,&T), exp:&Exp) -> (bool, Vec<String>) {
  let &(dest1,dest2) = dest;
  match *exp {
    Exp::Mov(ref id) if id == src && is_reg(dest1) => {
        assert!(*dest2!=T::Unit);
        (false, vec!{dest1.clone()})
    }
    Exp::IfEq(_, _, ref e1, ref e2) => {
      let (c1,mut rs1) = target_e(src, dest, e1);
      let (c2,rs2) = target_e(src, dest, e2);
      rs1.extend(rs2);
      (c1 && c2, rs1)
    }
    Exp::Call(_, ref ids) => (true, target_args(src, REGS, ids)),      
    _ => (false, vec!()),
  }
}
// register targeting
fn target_e(src:&String, dest:&(&String,&T), e:&E) -> (bool, Vec<String>) {
  match *e {
    E::Ans(ref exp) => target_exp(src, dest, exp),
    E::Let(ref id, ref t, ref exp, ref e) => {
      let (c1, mut rs1) = target_exp(src, &(id, t), exp);
      if c1 {
        return (true, rs1);
      }
      let (c2, rs2) = target_e(src, dest, e);
      rs1.extend(rs2);
      return (c2, rs1);
    }
  }
}

// "register sourcing" (?) as opposed to register targeting
// （x86の2オペランド命令のためのregister coalescing
fn source_exp(t:&T, exp:&Exp) -> Vec<String> {
  match *exp {
    Exp::Mov(ref x) | Exp::Neg(ref x) | Exp::Add(ref x, IdOrImm::C(_)) |
    Exp::Sub(ref x, _) => vec![x.clone()],
    Exp::Add(ref x, IdOrImm::V(ref y)) => vec![x.clone(), y.clone()],
    Exp::IfEq(_, _, ref e1, ref e2) | Exp::IfLE(_, _, ref e1, ref e2) |
    Exp::IfGE(_, _, ref e1, ref e2) => {
      let mut v = source_e(t, e1);
      v.extend(source_e(t, e2));
      v
    }
    Exp::Call(_, _) =>
      match *t {
        T::Unit => vec![],
        _ => vec![format!("{}",REGS[0])],
      },
    _ => vec![],
  }
}
fn source_e(t:&T, e:&E) -> Vec<String> {
  match *e {
    E::Ans(ref exp) => source_exp(t, exp),
    E::Let(_, _, _, ref e) => source_e(t, e),
  }
}

fn alloc(cont:&E, regenv:HashMap<String,String>, x:&String, t:&T, prefer:Vec<String>) -> Result<String,String> {
  // allocate a register or spill a variable
  assert!(!regenv.contains_key(x));
  
  let all =
    match *t {
      T::Unit => vec![], // dummy
      _ => {
         let mut vector = Vec::new();
         for i in REGS.iter() {
             vector.push(format!("{}",i))
         }
         vector          
      }
    };

  if all.len() == 0 { return Ok(format!("%unit")); } // [XX] ad hoc
  if is_reg(x) { return Ok(x.clone()); }
  let free = fv(cont);

  let mut live:HashSet<String> = HashSet::new(); // 生きているレジスタ
  for y in free.iter() {
    if is_reg(y) {
      live.insert(y.clone());
    } else {
      match regenv.get(y) {
        Some(v) => {live.insert(v.clone());()}
        None => {}
      };
    }
  }
  for r in prefer.iter() {
    if live.get(r)==None { return Ok(r.clone()) }
  }
  for r in all.iter() {
    if live.get(r)==None { return Ok(r.clone()) }
  }

  // Format.eprintf "register allocation failed for %s@." x;
  for y in free.iter().rev() {
    if !is_reg(y) {
      match regenv.get(y) {
        Some(v) =>
          if all.iter().position(|a| a.eq(v)) != None {
            //fprintf(stderr, "spilling %s from %s\n", y.c_str(), regenv.find(y)->second.c_str());
            return Err(y.clone());
          },
        None => {}
      };
    }
  }
  assert!(false);
  Err(format!(""))
}

// auxiliary function for walk_t and walk_exp_and_restore
fn add(x:&String, r:&String, mut regenv:HashMap<String,String>) -> HashMap<String,String> {
  if is_reg(x)  {
    assert!(*x == *r);
    regenv
  } else {
    regenv.insert(x.clone(),r.clone());
    regenv
  }
}
// auxiliary functions for walk_exp
#[derive(Debug)]
struct NoReg {
  id:String,
  t:T,
}

fn find(x:&String, t:&T, regenv:&HashMap<String,String>) -> Result<String,NoReg> {
  if is_reg(x) {
    Ok(x.clone())
  } else {
    match regenv.get(x) {
      None => Err(NoReg{id:x.clone(),t:t.clone()}),
      Some(x) => Ok(x.clone()),
    }
  }
}
fn find_imm(imm:&IdOrImm, regenv:&HashMap<String,String>) -> Result<IdOrImm,NoReg> {
  match *imm {
    IdOrImm::V(ref x) =>
      match find(x, &T::Int, regenv) {
        Ok(v) => Ok(IdOrImm::V(v)),
        Err(e) => Err(e), 
      },
    _ => Ok(imm.clone()),
  }
}
// ifのレジスタ割り当て
fn walk_exp_if(dest:&(&String,&T), cont:&E, regenv:HashMap<String,String>,
  constr:&Fn(E,E)->Exp, e1:&E, e2:&E) -> (E, HashMap<String,String>) {
  let (e1,regenv1) = walk_e(dest, cont, regenv.clone(), e1);
  let (e2,regenv2) = walk_e(dest, cont, regenv.clone(), e2);
  let mut regenv_ = HashMap::new(); // 両方に共通のレジスタ変数だけ利用
  for x in fv(cont).iter() {
    if is_reg(x) { continue; }
    match regenv1.get(x) { None => continue, Some(r1) =>
    match regenv2.get(x) { None => continue, Some(r2) =>
      if r1 != r2 { continue; }
      else {regenv_.insert(x.clone(), r1.clone());},
    }}
  }
  // そうでない変数は分岐直前にセーブ
  let mut e = E::Ans(constr(e1, e2));
  
  let &(dest1,_) = dest;
  for x in fv(cont).iter() {
    if *x == *dest1 || regenv_.get(x) != None { continue; } 
    match regenv.get(x) { None => (), Some(xx) => {
      e = seq(Exp::Save(xx.clone(), x.clone()), e);
    }}
  }
  (e, regenv_)
}

// 関数呼び出しのレジスタ割り当て
fn walk_exp_call(dest:&(&String,&T), cont:&E, regenv:HashMap<String,String>,
  constr:&Fn(Vec<String>)->Exp, ys:&Vec<String>) -> Result<(E, HashMap<String,String>),NoReg> {
  let &(dest1, _) = dest;
  let mut ys2 = vec!();
  for y in ys.iter() {
    match find(y, &T::Int, &regenv) {
      Err(err) => {return Err(err);},
      Ok(y) => {ys2.push(y);},
    }
  }
  let mut e = E::Ans(constr(ys2));
  for x in fv(cont).iter() {
    if *x == *dest1 { continue; }
    match regenv.get(x) {
      None => { continue; },
      Some(v) => {
        e = seq(Exp::Save(v.clone(), x.clone()), e);
      }
    }
  }
  Ok((e, HashMap::new()))
}

// 各命令のレジスタ割り当て
fn walk_exp(dest:&(&String,&T), cont:&E, regenv:HashMap<String,String>,
  exp:&Exp) -> Result<(E, HashMap<String,String>),NoReg> {
  match *exp {
    Exp::Nop | Exp::Set(_) | Exp::SetL(_) | Exp::Restore(_) =>
      Ok((E::Ans(exp.clone()), regenv)),
    Exp::Mov(ref x) => match find(x, &T::Int, &regenv) {
        Ok(v) => Ok((E::Ans(Exp::Mov(v)), regenv)),
        Err(e) => Err(e),
      },
    Exp::Neg(ref x) => match find(x, &T::Int, &regenv) {
        Ok(v) => Ok((E::Ans(Exp::Neg(v)), regenv)),
        Err(e) => Err(e),
      },
    Exp::Add(ref x, ref y) =>
      match find(x, &T::Int, &regenv) { Err(e) => Err(e), Ok(x) =>
      match find_imm(y, &regenv) { Err(e) => Err(e), Ok(y) =>
        Ok((E::Ans(Exp::Add(x,y)), regenv)),
      }},
    Exp::Sub(ref x, ref y) =>
      match find(x, &T::Int, &regenv) { Err(e) => Err(e), Ok(x) =>
      match find_imm(y, &regenv) { Err(e) => Err(e), Ok(y) =>
        Ok((E::Ans(Exp::Sub(x,y)), regenv)),
      }},
    Exp::Ld(ref x, ref y, i) =>
      match find(x, &T::Int, &regenv) { Err(e) => Err(e), Ok(x) =>
      match find_imm(y, &regenv) { Err(e) => Err(e), Ok(y) =>
        Ok((E::Ans(Exp::Ld(x,y,i)), regenv)),
      }},
    Exp::St(ref x, ref y, ref z, i) =>
      match find(x, &T::Int, &regenv) { Err(e) => Err(e), Ok(x) =>
      match find(y, &T::Int, &regenv) { Err(e) => Err(e), Ok(y) =>
      match find_imm(z, &regenv) { Err(e) => Err(e), Ok(z) =>
        Ok((E::Ans(Exp::St(x,y,z,i)), regenv)),
      }}},
    Exp::IfEq(ref x, ref y, ref e1, ref e2) =>
      match find(x, &T::Int, &regenv) { Err(e) => Err(e), Ok(x) =>
      match find_imm(y, &regenv) { Err(e) => Err(e), Ok(y) =>
        Ok(walk_exp_if(dest, cont, regenv.clone(), &(|e1:E, e2:E| Exp::IfEq(x.clone(), y.clone(), Box::new(e1), Box::new(e2))), e1, e2))
      }},
    Exp::IfLE(ref x, ref y, ref e1, ref e2) =>
      match find(x, &T::Int, &regenv) { Err(e) => Err(e), Ok(x) =>
      match find_imm(y, &regenv) { Err(e) => Err(e), Ok(y) =>
        Ok(walk_exp_if(dest, cont, regenv.clone(), &(|e1:E, e2:E| Exp::IfLE(x.clone(), y.clone(), Box::new(e1), Box::new(e2))), e1, e2))
      }},
    Exp::IfGE(ref x, ref y, ref e1, ref e2) =>
      match find(x, &T::Int, &regenv) { Err(e) => Err(e), Ok(x) =>
      match find_imm(y, &regenv) { Err(e) => Err(e), Ok(y) =>
        Ok(walk_exp_if(dest, cont, regenv.clone(), &(|e1:E, e2:E| Exp::IfGE(x.clone(), y.clone(), Box::new(e1), Box::new(e2))), e1, e2))
      }},
    Exp::Call(ref l, ref ys) =>
      walk_exp_call(dest, cont, regenv, &(|ys| Exp::Call(l.clone(), ys)), ys),
    Exp::Save(ref x, _) => {
      assert!(false);
      Err(NoReg{id:x.clone(),t:T::Unit})
    }
  }
}
// 使用される変数をスタックからレジスタへRestore
fn walk_exp_and_restore(dest:&(&String,&T), cont:&E, regenv:HashMap<String,String>,
  exp:&Exp) -> (E, HashMap<String,String>) {
  match walk_exp(dest, cont, regenv.clone(), exp) {
    Ok(r) => r,
    Err(NoReg{id:x, t}) => {
      // Format.eprintf "restoring %s@." x;
      walk_e(dest, cont, regenv, &E::Let(x.clone(), t, Exp::Restore(x), Box::new(E::Ans(exp.clone()))))
    }
  }
}
fn concat(e1:E, x:String, t:T, e2:E) -> E {
  match e1 {
    E::Ans(exp) => E::Let(x, t, exp, Box::new(e2)),
    E::Let(id, t1, exp, e) => E::Let(id, t1, exp, Box::new(concat(*e, x, t, e2))),
  }
}

// 命令列のレジスタ割り当て
fn walk_e(dest:&(&String,&T), cont:&E, regenv:HashMap<String,String>,
  e:&E) -> (E, HashMap<String,String>) {
  match *e {
    E::Ans(ref exp) => walk_exp_and_restore(dest, cont, regenv, exp),
    E::Let(ref x, ref t, ref exp, ref e) => {
      if regenv.get(x)!=None {
        println!("x={}", x);
      }
      assert!(regenv.get(x)==None);
      let &(dest1,dest2) = dest;
      let cont0 = &concat((**e).clone(), dest1.clone(), dest2.clone(), cont.clone());
      let (e1_, regenv1) = walk_exp_and_restore(&(x,t), cont0, regenv.clone(), exp);
      let (_call, mut targets) = target_e(x, dest, cont0);
      targets.extend(source_e(t, &e1_));
      // レジスタ間のmovよりメモリを介するswapのほうが問題なので、sourcesよりtargetsを優先
      match alloc(cont0, regenv1.clone(), x, t, targets) {
        Err(y) =>
          match regenv1.get(&y) {
            None => {assert!(false);panic!("err");}
            Some(r) => {
              let mut regenv2 = regenv1.clone();
              regenv2.remove(&y);
              let (e2_, regenv2) =
                walk_e(dest, cont, add(x, r, regenv2), &*e);
              let save = match regenv.get(&y) {
                Some(a) => Exp::Save(a.clone(), y),
                None => Exp::Nop,
              };
              (seq(save, concat(e1_, r.clone(), t.clone(), e2_)), regenv2)
            }
          },
        Ok(r) => {
          let (e2_, regenv2) = walk_e(dest, cont, add(x, &r, regenv1), &*e);
          (concat(e1_, r, t.clone(), e2_), regenv2)

        }
      }
    }
  }
}
// 関数のレジスタ割り当て
fn walk_fundef(fundef:&Fundef) -> Fundef {
  match *fundef {
    Fundef::Fundef(ref name, ref args, ref body, ref ret) => {
      let mut regenv = HashMap::new();
      regenv.insert(name.clone(), format!("{}",REG_CL));
      let mut i = 0;
      let mut arg_regs = vec!();
      // 今のレジスタ割り当てを表す（変数からレジスタへの）写像regenv
      for y in args.iter() {
        assert!(!is_reg(y));
        let r = REGS[i];
        i+=1;
        arg_regs.push(format!("{}", r));
        regenv.insert(y.clone(), format!("{}", r));
      }
      let a = match *ret {
        T::Unit => gentmp(ret),
        _ => format!("{}", REGS[0]),
      };
      let (e_, _) = walk_e(&(&a, ret), &E::Ans(Exp::Mov(a.clone())), regenv, body);
      Fundef::Fundef(name.clone(), arg_regs, e_, ret.clone())
    }
  }
}
// プログラム全体のレジスタ割り当て
fn walk_prog(prog:&Prog) -> Prog {    
  match *prog {
    Prog::Prog(ref fundefs, ref e) => {
      //Format.eprintf "register allocation: may take some time (up to a few minutes, depending on the size of functions)@.";
      let fundefs = fundefs.iter().map(|x| walk_fundef(x)).collect();
      let (e, _) =
        walk_e(&(&gentmp(&T::Unit), &T::Unit), &E::Ans(Exp::Nop), HashMap::new(), e);
      Prog::Prog(fundefs, e)
    }
  }
}

pub fn reg_alloc(prog:&Prog) -> Prog {
  walk_prog(prog)
}
