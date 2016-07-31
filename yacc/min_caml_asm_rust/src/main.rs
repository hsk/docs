pub mod parser;
pub mod ast;
pub mod ast_print;
pub mod simm;
pub mod reg_alloc;
pub mod emit;

fn main() {
  use ast_print::*;

  use std::io::prelude::*;
  use std::fs::File;

  let mut f = match File::open("test/fib.mls") {
  	Err(why) => panic!("open error {}", why),
  	Ok(f) => f,
  };
  let mut s = String::new();
  match f.read_to_string(&mut s) {
  	Err(why) => panic!("read error {:?}", why),
  	Ok(_) => (),
  }

  println!("{}", s);
  let prog = match parser::parse_program(&*s) {
    Err(why) => panic!("parse error {:?}", why),
    Ok(prog) => prog,
  };
  println!("prog = {}", prog.p());
  let prog = simm::simm(&prog);
  println!("simm {}",prog.p());
  let prog = reg_alloc::reg_alloc(&prog);
  println!("reg_alloc {}",prog.p());
  emit::emit(&format!("a.s"), &prog);
  println!("{:?}",exec("gcc -m32 a.s stub.c x86_libmincaml.s -o a.exe"));
  println!("{:?}",exec("./a.exe"));
}

fn exec(cmd:&str) -> (i32, String, String) {
  use std::process::*;
  let mut cmds:Vec<&str> = cmd.split(' ').collect();
  let mut cmd = Command::new(cmds.remove(0));
  for arg in cmds.iter() {
    cmd.arg(*arg);
  }
  let err = "failed to execute process";
  let output = cmd.output().expect(err);
  (cmd.status().expect(err).code().expect(err),
  String::from_utf8(output.stdout).expect(err),
  String::from_utf8(output.stderr).expect(err)
  )
}


