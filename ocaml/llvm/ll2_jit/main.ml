open Ast
open Utils

exception Error of string

let parse str =
  let lexbuf = Lexing.from_string (str^"\n") in
  Parser.main Lexer.token lexbuf

module Compile = struct

  let vs = ref ([] : v list)
  let add(v:v):unit = vs := v::!vs
  
  let rec visit(e: e): r =
    match e with
    | EInt(i) -> RN(Ti 64,string_of_int i)
    | EBin(e1, op, e2) ->
      let reg1 = visit(e1) in
      let reg2 = visit(e2) in
      let reg3 = RL(Ti 64,genid("..")) in
      add(VBin(reg3, op, reg1, reg2));
      reg3

  let apply(e: e): v list =
    vs := [];
    let reg = visit(e) in
    add(VPrint(reg));
    List.rev !vs
end

module Emit = struct

  let context = Llvm.global_context ()
  let m = Llvm.create_module context "module"

  let i8_type = Llvm.i8_type context
  let i32_type = Llvm.i32_type context
  let i64_type = Llvm.i64_type context
  let double_type = Llvm.double_type context
  let void_type = Llvm.void_type context

  let map : (string * Llvm.llvalue) list ref = ref []
  let add id v = map := (id, v) :: !map
  let lookup id =
    try
      List.assoc id !map
    with
    | Not_found ->
      Printf.printf "Not found %s\n" id;
      raise Not_found

  let p (r: r): string =
    match r with
    | RL(_, id) -> id
    | RN(_, id) -> id

  let r (r1: r):Llvm.llvalue =
    match r1 with
    | RL(_, id) -> lookup (p r1)
    | RN(_, id) -> Llvm.const_int i64_type (int_of_string id)

  let t (t: t): Llvm.lltype =
    match t with
    | Tv     -> void_type
    | Ti(8)  -> i8_type
    | Ti(32) -> i32_type
    | Ti(64) -> i64_type

  let pt (t: t): string =
    match t with
    | Tv -> "void"
    | Ti i -> "i" ^ string_of_int i

  let prt (r: r): string =
    match r with
    | RL(t1, _) -> pt t1
    | RN(t1, _) -> pt t1

  let visit print_l b (v: v): unit = 
    match v with
    | VBin(id, "add", r1, r2) -> add (p id) (Llvm.build_add  (r r1) (r r2) (p id) b)
    | VBin(id, "sub", r1, r2) -> add (p id) (Llvm.build_sub  (r r1) (r r2) (p id) b)
    | VBin(id, "mul", r1, r2) -> add (p id) (Llvm.build_mul  (r r1) (r r2) (p id) b)
    | VBin(id, "div", r1, r2) -> add (p id) (Llvm.build_sdiv (r r1) (r r2) (p id) b)
    | VPrint(a) -> ignore(Llvm.build_call print_l [| r a |] "" b)

  let apply(vs: v list):Llvm.llmodule =
    let printf = Llvm.declare_function "printf"
      (Llvm.var_arg_function_type i32_type [| Llvm.pointer_type i8_type |]) m
    in
    let print_l = Llvm.define_function "print_l"
      (Llvm.function_type void_type [| i64_type |]) m
    in
    let b = Llvm.builder_at_end context (Llvm.entry_block print_l) in
    let str = Llvm.build_global_stringptr "%ld\n" "str" b in
    ignore (Llvm.build_call printf [| str; Llvm.param print_l 0 |] "" b);
    ignore (Llvm.build_ret_void b);
    let main = Llvm.define_function "main" (Llvm.function_type i32_type [| |]) m in
    let b = Llvm.builder_at_end context (Llvm.entry_block main) in
    List.iter (fun v -> visit print_l b v) vs;
    ignore (Llvm.build_ret (Llvm.const_int i32_type 0) b);
    m

end

module JIT = struct
  include Emit
  module LE = Llvm_executionengine

  let run m =
    ignore (LE.initialize ());
    let engine = LE.create m in
    LE.run_static_ctors engine;
    let main_cty = Foreign.funptr Ctypes.(void @-> returning void) in
    let main     = LE.get_function_address "main" main_cty engine in
    main ();
    LE.dispose engine;
    Gc.compact ()
end

let () =
  let ast = parse "1+2*3" in
  let codes = Compile.apply ast in
  let m = Emit.apply codes in
  Llvm.dump_module m;
  JIT.run m
