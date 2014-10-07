open Ty
open Exp
open Stmt
open Utils



let compile output input =
  let inp = open_in (input) in
  let lexbuf = Lexing.from_channel inp in
  let ast = Parser.stmts Lexer.token lexbuf in
  let (ast:Stmt.t) = cnv(SList(ast)) in
  let cpp = Stmt.print ast in
  Printf.printf "%s\n" cpp;
  asm_open output;
  asm(cpp);
  asm_close()

let _ =
  compile "a.cpp" "a.lll"
(*  print_exec("g++ a.cpp");
  print_exec("./a.out")
*)