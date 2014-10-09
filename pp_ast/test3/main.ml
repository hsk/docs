open Ty
open Exp
open Stmt
open Utils



let compile output input =
  let inp = open_in (input) in
  let lexbuf = Lexing.from_channel inp in
  let ast = Parser.stmts Lexer.token lexbuf in
Printf.printf "parse ok@.\n"; flush stdout;

  let (ast:Stmt.t) = cnv(SList(ast)) in
Printf.printf "cnv ok@.\n"; flush stdout;
  let buf = Buffer.create 1024 in
  let formatter = Format.formatter_of_buffer buf in
  Stmt.print formatter ast;
  Format.fprintf formatter "@?";
  let cpp = Buffer.contents buf in

Printf.printf "print ok\n"; flush stdout;
(*  Printf.printf "%s\n" cpp;*)
  asm_open output;
  asm(cpp);
  asm_close();
  Printf.printf "end compile\n"; flush stdout

let _ =
  compile "a.cpp" "a.lll"
(*  print_exec("g++ a.cpp");
  print_exec("./a.out")
*)
