(*|

以下のプログラムを出力するASTを作成する。

```
#include <stdio.h>
#include <sys/time.h>
#include <memory.h>

long gett() {
  timeval tv;
  gettimeofday (&tv, NULL);
  return (tv.tv_sec) * 1000 + tv.tv_usec / 1000;
}

int fib(int n) {
  if(n < 2) return 1;
  return fib(n - 2) + fib(n - 1);
}

int main() {
  long start = gett();
  printf("%d\n", fib(40));
  printf("%ld\n", gett() - start);
  return 0;
}
```



*)


let rec print_ls ?(sep="(;)\n") p = function
  | [] -> ""
  | [x] -> p x
  | x::xs -> Printf.sprintf "%s%s%s" (p x) sep (print_ls p xs ~sep:sep)

module Ty = struct
  type t =
  | Ty of string

  let print sp t =
    match t with
    | Ty(s) -> s

end

module Exp = struct
  type e =
    | EInt of int
    | EBin of string * e * e
    | EPre of string * e
    | ECall of e * e list
    | EVar of string
    | EString of string
    | EEmpty
  let rec print sp e =
    match e with
    | EInt i -> Printf.sprintf "%s%d" sp i
    | EVar i -> Printf.sprintf "%s%s" sp i
    | EString i -> Printf.sprintf "%s%s" sp i
    | EBin(op,e1,e2) -> Printf.sprintf "%s(%s %s %s)" sp (print "" e1) op (print "" e2)
    | EPre(op,e1) -> Printf.sprintf "%s(%s%s)" sp op (print "" e1)
    | ECall(e1,es) -> Printf.sprintf "%s(%s)" (print sp e1) (print_ls (print "") es ~sep:", ")
    | EEmpty -> ""  
end

module Stmt = struct
  type t = 
    | SBlock of t list
    | SIf of Exp.e * t * t
    | SEmpty
    | SExp of Exp.e
    | SRet of Exp.e
    | SFun of Ty.t * string * (Ty.t * string) list * t
    | SInclude of string
    | SLet of Ty.t * string * Exp.e

  let print (t:t):string = 
    let rec print sp e =
    match e with
    | SBlock ls -> Printf.sprintf "%s{\n%s\n%s}" sp (print_ls ~sep:"\n" (print (sp^"  "))ls) sp
    | SIf(e1,e2,SEmpty) ->
      Printf.sprintf "%sif (%s)%s%s"
        sp
        (Exp.print "" e1)
        (print2 sp e2 "\n")
        sp
    | SIf(e1,e2,e3) ->
      Printf.sprintf "%sif (%s)%s%selse%s"
        sp
        (Exp.print "" e1)
        (print2 sp e2 "\n")
        sp
        (print2 sp e3 "")
    | SFun (t, id, ts, e2) ->
      Printf.sprintf "%s %s(%s)%s"
        (Ty.print "" t)
        id
        (print_ls (fun (t,a)-> Printf.sprintf "%s %s" (Ty.print "" t) a) ts ~sep:", " )
        (print2 sp e2 "\n")
      
    | SEmpty -> ""
    | SExp e -> Exp.print sp e ^ ";"
    | SRet e -> sp ^ "return " ^ Exp.print "" e ^ ";"
    | SInclude s -> "#include " ^ s
    | SLet (t, id, Exp.EEmpty) -> Printf.sprintf "%s%s %s" sp (Ty.print "" t) id ^ ";"
    | SLet (t, id, e) -> Printf.sprintf "%s%s %s = %s" sp (Ty.print "" t) id (Exp.print "" e) ^ ";"
    and print2 sp e ed = 
      match e with
      | SBlock ls -> " " ^ print sp e ^ (if ed <> "" then " " else "")
      | _ -> "\n" ^ print (sp^"  ") e ^ ed
  
    in print "" t

  let prints (ts:t list): string =
    print_ls ~sep:"\n" print ts

end

open Exp
open Stmt
open Ty
let _ =
  let e = [

    (*#include <stdio.h>*)
    SInclude "<stdio.h>";
    (*#include <sys/time.h>*)
    SInclude "<sys/time.h>";
    (*#include <memory.h>*)
    SInclude "<memory.h>";

    (*
    long gett() {
      timeval tv;
      gettimeofday (&tv, NULL);
      return (tv.tv_sec) * 1000 + tv.tv_usec / 1000;
    }
    *)
    SFun(Ty "long", "gett",[],SBlock[
      SLet(Ty "timeval", "tv", EEmpty);
      SExp(ECall(EVar "gettimeofday", [ EPre("&", EVar "tv"); EVar "NULL"]));
      SRet(EBin("+",
        EBin("*",EBin(".",EVar "tv",EVar "tv_sec"), EInt 1000),
        EBin("/", EBin(".", EVar "tv", EVar "tv_usec"), EInt 1000)))
    ]);

    (*
    int fib(int n) {
      if(n < 2) return 1;
      return fib(n - 2) + fib(n - 1);
    }
    *)
    SFun(Ty "int", "fib",[Ty "int","n"],SBlock[
      SIf(EBin("<", EVar "n",EInt 2),SRet(EInt 1), SEmpty);
      SRet(EBin("+",
        ECall(EVar "fib", [EBin("-", EVar "n", EInt 2)]),
        ECall(EVar "fib", [EBin("-", EVar "n", EInt 1)])))
    ]);
    (*
    int main() {
      long start = gett();
      printf("%d\n", fib(40));
      printf("%ld\n", gett() - start);
      return 0;
    }
    *)

    SFun(Ty "int", "main",[],SBlock[
      SLet(Ty "long","start", ECall(EVar "gett",[]));
      SExp(ECall(EVar "printf",[EString "\"%d\\n\""; ECall(EVar "fib",[EInt 40])]));
      SExp(ECall(EVar "printf",[EString "\"%ld\\n\""; EBin("-",ECall(EVar "gett",[]),EVar "start")]));
      SRet(EInt 0)
    ])
  ] in
  Printf.printf "%s\n" (prints e);
