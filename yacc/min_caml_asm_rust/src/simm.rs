use ast::*;
use std::collections::HashMap;
/*
fn t(f:&Fn(String,IdOrImm,Box<E>,Box<E>)->Exp) -> Exp {
  f(format!("tes"),IdOrImm::C(1), Box::new(E::Ans(Exp::Nop)), Box::new(E::Ans(Exp::Nop)))
}

fn t2() -> Exp {
  t(&Exp::IfEq)
}
*/
fn walk_exp_if(env:&HashMap<String,i32>, id:&String, imm:&IdOrImm, e1:&E, e2:&E, f:&Fn(String,IdOrImm,Box<E>,Box<E>)->Exp) -> Exp {
  match *imm {
    IdOrImm::V(ref v) =>
      match env.get(v) { 
        Some(i) => f(id.clone(), IdOrImm::C(*i), Box::new(walk_e(env, e1)), Box::new(walk_e(env, e2))),
        None =>
          match env.get(id) {
            Some(ii) =>
              f(v.clone(), IdOrImm::C(*ii), Box::new(walk_e(env, e1)), Box::new(walk_e(env, e2))),
            None =>
              f(id.clone(), imm.clone(), Box::new(walk_e(env, e1)), Box::new(walk_e(env, e2))),
          },
      },
    _ => f(id.clone(), imm.clone(), Box::new(walk_e(env, e1)), Box::new(walk_e(env, e2))),
  }
}

// 各命令の即値最適化
fn walk_exp(env:&HashMap<String,i32>, e:&Exp) -> Exp {
  // 環境に変数の即値があれば、即値に置き換える
  match *e {
    Exp::Add(ref id, IdOrImm::V(ref imm)) =>
      match env.get(imm) {
        Some(&ii) => Exp::Add(id.clone(), IdOrImm::C(ii)),
        None =>
          match env.get(id) {
            Some(&ii) => Exp::Add(imm.clone(),IdOrImm::C(ii)),
            None => e.clone(),
          }
      },
    Exp::Sub(ref id, IdOrImm::V(ref imm)) =>
      match env.get(imm) {
        Some(&ii) => Exp::Sub(id.clone(), IdOrImm::C(ii)),
        None => e.clone(),
      },
    Exp::Ld(ref id, IdOrImm::V(ref imm), i) =>
      match env.get(imm) {
        Some(&ii) => Exp::Ld(id.clone(), IdOrImm::C(ii), i),
        None => e.clone(),
      },
    Exp::St(ref id, ref id2, IdOrImm::V(ref imm), i) =>
      match env.get(imm) {
        Some(&ii) => Exp::St(id.clone(), id2.clone(), IdOrImm::C(ii), i),
        None => e.clone(),
      },
    // 分岐命令の場合は両方を変換する
    Exp::IfEq(ref id, ref imm, ref e1, ref e2) =>
      walk_exp_if(env, id, imm, e1, e2, &Exp::IfEq),
    Exp::IfLE(ref id, ref imm, ref e1, ref e2) =>
      walk_exp_if(env, id, imm, e1, e2, &Exp::IfLE),
    Exp::IfGE(ref id, ref imm, ref e1, ref e2) =>
      walk_exp_if(env, id, imm, e1, e2, &Exp::IfGE),
    // 他の命令はそのまま
    _ => e.clone(),
  }
}
// 命令列の即値最適化
fn walk_e(env:&HashMap<String,i32>, e:&E) -> E {
  match *e {
    E::Ans(ref exp) => E::Ans(walk_exp(env, exp)),
    // 即値なら
    E::Let(ref id, ref t, Exp::Set(ref i), ref e1) => {
        // 環境に保存して後続を計算
        let mut env2 = env.clone();
        env2.insert(id.clone(), *i);
        let e0 = walk_e(&env2, e1);
        // 後続でレジスタを使っていれば、letを残すが、使っていなければ消す
        let vec = fv(&e0);
        if !vec.contains(id) {
          e.clone()
        } else {
          E::Let(id.clone(), t.clone(), Exp::Set(*i), Box::new(e0))
        }
      },
    // 即値でなければ、そのまま
    E::Let(ref id, ref t, ref exp, ref e) =>
      E::Let(id.clone(), t.clone(), walk_exp(env, exp), Box::new(walk_e(env, &e))),
  }
}

// トップレベル関数の即値最適化
fn walk_fundef(f:&Fundef) -> Fundef {
  match *f {
    Fundef::Fundef(ref name, ref args, ref body, ref ret) =>
      Fundef::Fundef(name.clone(), args.clone(),
        walk_e(&HashMap::new(), body), ret.clone()),
  }
}

// プログラム全体の即値最適化
fn walk_prog(prog:&Prog) -> Prog {
  match *prog {
    Prog::Prog(ref fundefs, ref e) => {
      let mut v = vec!();
      for fundef in fundefs {
        v.push(walk_fundef(fundef))
      }
      Prog::Prog(v, walk_e(&HashMap::new(),e))
    }
  }
}

pub fn simm(prog:&Prog) -> Prog {
  walk_prog(prog)
}
