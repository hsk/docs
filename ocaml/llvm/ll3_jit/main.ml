open Ast

module LE = Llvm_executionengine

let context = Llvm.global_context ()
let m = Llvm.create_module context "module"

(* i8,i32,i64,doubleの型 *)
let i8_type = Llvm.i8_type context
let i32_type = Llvm.i32_type context
let i64_type = Llvm.i64_type context
let double_type = Llvm.double_type context
let void_type = Llvm.void_type context

exception Error of string

let parse str =
  let lexbuf = Lexing.from_string (str^"\n") in
  Parser.main Lexer.token lexbuf

let compile (fn:Llvm.llvalue) (b:Llvm.llbuilder) (e:e): Llvm.llvalue =
  let rec visit(e: e): Llvm.llvalue =
    match e with
    | EInt(i) -> Llvm.const_int i64_type i
    | EBin(e1, op, e2) ->
      let reg1 = visit e1 in
      let reg2 = visit e2 in
      match op with
      | "add" -> Llvm.build_add reg1 reg2 "" b
      | "sub" -> Llvm.build_sub reg1 reg2 "" b
      | "mul" -> Llvm.build_mul reg1 reg2 "" b
      | "div" -> Llvm.build_sdiv reg1 reg2 "" b
      | _ -> assert false
  in
  visit e

let compile e =
  let printf = Llvm.declare_function "printf"
    (Llvm.var_arg_function_type i32_type [| Llvm.pointer_type i8_type |])  m in
  let print_l = Llvm.define_function "print_l"
    (Llvm.function_type void_type [| i64_type |]) m in
  let b = Llvm.builder_at_end context (Llvm.entry_block print_l) in
  let str = Llvm.build_global_stringptr "%ld\n" "str" b in
  ignore (Llvm.build_call printf [| str; Llvm.param print_l 0 |] "" b);
  ignore (Llvm.build_ret_void b);
  let main = Llvm.define_function "main" (Llvm.function_type i32_type [| |]) m in
  let b = Llvm.builder_at_end context (Llvm.entry_block main) in
  ignore(Llvm.build_call print_l [| compile main b e |] "" b);
  ignore (Llvm.build_ret (Llvm.const_int i32_type 0) b)

let run() =
  ignore (LE.initialize ());
  let engine = LE.create m in
  LE.run_static_ctors engine;
  let main_cty = Foreign.funptr Ctypes.(void @-> returning void) in
  let main     = LE.get_function_address "main" main_cty engine in
  main ();
  LE.dispose engine;
  Gc.compact ()

let () =
  let ast = parse "1+2*3" in
  compile ast;
  Llvm.dump_module m;
  run()
