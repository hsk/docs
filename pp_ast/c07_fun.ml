let rec print_ls ?(sep=";\n") p = function
  | [] -> ""
  | [x] -> p x
  | x::xs -> Printf.sprintf "%s%s%s" (p x) sep (print_ls p xs)

module Exp = struct
  type e = {raw:raw;pos:int}
  and raw =
    | EInt of int
    | EBin of string * e * e
    | ECall of e * e list
    | EVar of string
  let e raw = {raw=raw;pos=0}

  let rec print sp {raw}=
    match raw with
    | EInt i -> Printf.sprintf "%s%d" sp i
    | EVar i -> Printf.sprintf "%s%s" sp i
    | EBin(op,e1,e2) -> Printf.sprintf "%s(%s %s %s)" sp (print "" e1) op (print "" e2)
    | ECall(e1,es) -> Printf.sprintf "%s(%s)" (print sp e1) (print_ls (print "") es ~sep:", ")
  
end

module Stmt = struct
  type t = {raw:raw;pos:int}
  and raw =
    | SBlock of t list
    | SIf of Exp.e * t * t
    | SEmpty
    | SExp of Exp.e
    | SRet of Exp.e
    | SFun of string * string list * t
  let r raw = {raw=raw;pos=0}

  let print (t:t):string = 
    let rec print sp {raw}=
    match raw with
    | SBlock ls -> Printf.sprintf "%s{\n%s\n%s}" sp (print_ls ~sep:"\n" (print (sp^"  "))ls) sp
    | SIf(e1,e2,e3) ->
      Printf.sprintf "if (%s)%s%selse%s"
        (Exp.print "" e1)
        (print2 sp e2 "\n")
        sp
        (print2 sp e3 "")
    | SFun (id, ss, e2) ->
      Printf.sprintf "function %s(%s)%s"
        id
        (print_ls (fun a->a) ss ~sep:", " )
        (print2 sp e2 "\n")
      
    | SEmpty -> ""
    | SExp e -> Exp.print sp e ^ ";"
    | SRet e -> sp ^ "return " ^ Exp.print "" e ^ ";"
    and print2 sp ({raw}as e) ed = 
      match raw with
      | SBlock ls -> " " ^ print sp e ^ (if ed <> "" then " " else "")
      | _ -> "\n" ^ print (sp^"  ") e ^ ed
  
    in print "" t

end

open Exp
open Stmt

let _ =
  let e1 = e(EBin("+", e(EInt(1)), e(EInt(2)))) in
  let ee = r(SExp(e1)) in
  Printf.printf "%s\n" (print ee);
  let e2:Stmt.t = r(SBlock[ee;ee]) in
  Printf.printf "%s\n" (print e2);
  let e3 = r(SIf(e1,ee,ee)) in
  Printf.printf "%s\n" (print e3);

  let e4 = r(SIf(e1,e2,e2)) in
  Printf.printf "%s\n" (print e4);
  let ec:Stmt.t = r(SExp(
    e(ECall(e(ECall(e(EVar("ff")), [e(EInt(2));e(EInt(3))])), [e(EInt(2));e(EInt(3))]))
  )) in  

  let e5 = r(SIf(e1,e2,ec)) in
  Printf.printf "%s\n" (print e5);
  let e6 = r(SFun("add",["a";"b"],r(SBlock[r(SRet(e(EBin("+",e(EVar "a"),e(EVar "b")))))]))) in
  Printf.printf "%s\n" (print e6);
