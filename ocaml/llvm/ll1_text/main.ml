open Ast
open Utils

exception Error of string

let parse str =
  let lexbuf = Lexing.from_string (str^"\n") in
  Parser.main Lexer.token lexbuf

module Compile = struct

  let vs = ref ([] : v list)

  let add(v:v):unit =
    vs := v::!vs
  
  let rec visit(e: e): r =
    match e with
    | EInt(i) ->
      RN(Ti 64,string_of_int i)
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

let compile e = Compile.apply(e)

module Emit = struct

  let p(r:r): string =
    match r with
    | RL(_, id) -> "%" ^ id
    | RN(_, id) -> id

  let t(t:t): string =
    match t with
    | Tv   -> "void"
    | Ti i -> "i" ^ string_of_int i

  let pt(r:r): string =
    match r with
    | RL(t1, _) -> t t1
    | RN(t1, _) -> t t1

  let visit(v: v):unit = 
    match v with
    | VBin(id, "div", a, b) ->
      Asm.p(p(id) ^ " = sdiv "^pt(a)^" " ^ p(a) ^ ", " ^ p(b))
    | VBin(id, op, a, b) ->
      Asm.p(p(id) ^ " = " ^ op ^ " "^pt(a)^" " ^ p(a) ^ ", " ^ p(b))
    | VPrint(a) ->
      Asm.p("call void @print_l("^pt(a)^" " ^ p(a) ^ ") nounwind ssp")

  let apply((file: string), (vs: v list)):unit =
    Asm.openf(file);

    Asm.label("define i32 @main() nounwind ssp {");
    Asm.label("entry:");
    List.iter (fun v -> visit v) vs;
    Asm.p("ret i32 0");
    Asm.label("}");

    Asm.label("@.str = private constant [5 x i8] c\"%ld\\0A\\00\"");
    Asm.label("define void @print_l(i64 %a) nounwind ssp {");
    Asm.label("entry:");
    Asm.p("%a_addr = alloca i64, align 8");
    Asm.p("store i64 %a, i64* %a_addr");
    Asm.p("%0 = load i64* %a_addr, align 8");
    Asm.p("%1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @.str, i64 0, i64 0), i64 %0) nounwind");
    Asm.p("ret void");
    Asm.label("}");
    Asm.label("declare i32 @printf(i8*, ...) nounwind");
    Asm.close()
end

let emit e = Emit.apply(e)

let _ =
  let ast = parse "1+2*3" in
  let codes = compile(ast) in
  emit("out.ll", codes);
  match exec("llc-3.5 out.ll -o out.s") with
  | (a,b,c) -> print_string("(" ^ a ^ "," ^ b ^ "," ^ c ^ ")\n");
  match exec("clang -m64 out.s -o out.exe") with
  | (a,b,c) -> print_string("(" ^ a ^ "," ^ b ^ "," ^ c ^ ")\n");
  match exec("./out.exe") with
  | (a,b,c) -> print_string(a);
