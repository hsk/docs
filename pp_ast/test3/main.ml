open Ty
open Exp
open Stmt
open Utils



let compile input output =
  let inp = open_in (input) in
  let lexbuf = Lexing.from_channel inp in
  let ast = Parser.stmts Lexer.token lexbuf in
  let (ast:Stmt.t) = cnv(SList(ast)) in
  let buf = Buffer.create 1024 in
  let formatter = Format.formatter_of_buffer buf in
  Stmt.print formatter ast;
  Format.fprintf formatter "@?";
  let cpp = Buffer.contents buf in
  asm_open output;
  asm(cpp);
  asm_close()

let _ =

  compile Sys.argv.(1) Sys.argv.(2)
  (*  print_exec("g++ a.cpp");
  print_exec("./a.out")
*)
