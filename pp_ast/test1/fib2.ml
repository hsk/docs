(*|

以下のプログラムを出力するASTを作成する。

```
#include "fib2.h"
  // struct Int
  int Int_classId = Class_genId();
  struct Int {
    int id;
    int x;
    Int(int x):id(Int_classId),x(x){}
  };


  struct Fib2 {
    int (*fib)(Class*);
  };
  Vec* Fib2_v = newVec();

  int Fib2_Int_fib(Class* self) {
    Int* p = (Int* )self;
    if(p-> x < 2) return 1;

    Int p1(p->x - 2);
    Int p2(p->x - 1);

    return ((Fib2* )Fib2_v->data[p->id])->fib((Class* )&p1) +
        ((Fib2* )Fib2_v->data[p->id])->fib((Class* )&p2);
  }

  Fib2* newFib2_Int() {
    Fib2 *impl = new Fib2();
    setVec(Fib2_v, Int_classId, (void* )impl);
    impl->fib = &Fib2_Int_fib;
    return impl;
  }
  Fib2* Fib2_Int_ = newFib2_Int();

int main() {
    long start;
    start = gett();
    Int p(40);
    printf("%d\n", ((Fib2* )Fib2_v->data[p.id])->fib((Class* )&p));
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
  | TFun of t * t list

  let rec print ?(pp="") sp t =
    match t with
    | Ty(s) -> sp ^ s
    | TFun(r,ts) -> Printf.sprintf "%s(*%s)(%s)" (print sp r) pp (print_ls (print "") ts ~sep:", ")

end

module Exp = struct
  type e =
    | EInt of int
    | EBin of e * string * e
    | EPre of string * e
    | ECall of e * e list
    | ECallM of string * e * e list
    | EArr of e * e list
    | EVar of string
    | EString of string
    | EEmpty
    | ECast of Ty.t * e

  let rec print sp e =
    match e with
    | EInt i -> Printf.sprintf "%s%d" sp i
    | EVar i -> Printf.sprintf "%s%s" sp i
    | EString i -> Printf.sprintf "%s%s" sp i
    | EBin(e1,op,e2) -> Printf.sprintf "%s(%s %s %s)" sp (print "" e1) op (print "" e2)
    | EPre(op,e1) -> Printf.sprintf "%s(%s %s)" sp op (print "" e1)
    | ECall(e1,es) -> Printf.sprintf "%s(%s)" (print sp e1) (print_ls (print "") es ~sep:", ")
    | ECallM(i,e1,es) -> Printf.sprintf "%s(%s)" (print sp e1) (print_ls (print "") es ~sep:", ")
    | EArr(e1,es) -> Printf.sprintf "%s[%s]" (print sp e1) (print_ls (print "") es ~sep:", ")
    | ECast(t,e) -> Printf.sprintf "((%s)%s)" (Ty.print sp t) (print "" e)
    | EEmpty -> ""
  end

module Stmt = struct
  open Exp
  open Ty
  type t = 
    | SBlock of t list
    | SIf of Exp.e * t * t
    | SEmpty
    | SExp of Exp.e
    | SRet of Exp.e
    | SFun of Ty.t * string * (Ty.t * string) list * t
    | SInclude of string
    | SLet of Ty.t * Exp.e * Exp.e
    | SStruct of string * (Ty.t * t) list
    | SCon of (Ty.t * string) list * Exp.e list * t
    | STrait of  string * (Ty.t * t) list
    | SImpl of string * string * t list
    | SList of t list

  let rec cnve (e:e):e =
    match e with
    | EBin(e1,op,e2) -> EBin(cnve e1,op,cnve e2)
    | EPre(op,e1) -> EPre(op, cnve e1)
    | ECall(e1,es) -> ECall(cnve e1,List.map cnve es)
    | ECallM(i,e1,es) ->
      begin match cnve e1 with
      | EBin(e1,"->",fn) ->
        let ea = ECast(Ty (i^"*"),
          EArr(EBin(EVar(i^"_v"),"->",EVar "data"), [EBin(e1,".",EVar "id")])) in
        ECall(EBin(ea,"->",fn),(ECast(Ty "Class*", EPre("&",e1)))::(List.map cnve es))
      | t -> assert false
      end        
    | EArr(e1,es) -> ECall(cnve e1,List.map cnve es)
    | ECast(t,e) -> ECast(t, cnve e)
    | EInt i -> e
    | EString _ -> e
    | EVar _ -> e
    | EEmpty -> e

  let rec cnvfun i i2 (t:t):t =
    match t with
    | SFun(t1,id,ls,SBlock b) ->
      SFun(
        t1,
        i^"_"^i2^"_"^id,
        (Ty "Class*","self_")::ls,

        SBlock(
          (SLet(Ty (i2 ^ "*"), EVar "self", ECast(Ty (i2 ^ "*"), EVar "self_")))::
          (List.map (cnvfun i i2) b)
        )

      )
    | t -> cnv(t)

  and cnv(t:t):t =
    match t with
    | SList(ls) -> SList(List.map cnv ls)
    | STrait(id,ts) ->
      SList([
        SStruct(id,ts);
        (*
          Vec* Fib2_v = newVec();
        *)
        SLet(Ty "Vec*", EVar (id ^ "_v"), ECall(EVar"newVec",[]))
      ])
    | SImpl(id,id2,ss) ->
      let ss = List.map (cnvfun id id2) ss in
      SList(ss@[
        SFun(Ty(id^"*"), "new"^id^"_"^id2,[],SBlock[
          SLet(Ty(id), EPre("*", EVar "impl"), EPre("new",ECall(EVar id,[])));
          SExp(ECall(EVar "setVec", [ EVar(id^"_v"); EVar (id2^"_classId"); ECast(Ty "void*",EVar "impl") ]));
          SExp(EBin(EBin(EVar "impl", "->", EVar "fib"), "=", EPre("&", EVar(id^"_"^id2^"_fib"))));

          SRet(EVar "impl")
        ]);
        (*
          "^id^"* "^id^"_Int_ = new"^id^"_Int();
        *)
        SLet(Ty(id^"*"), EVar (id^"_"^id2^"_"), ECall(EVar("new"^id^"_"^id2),[]))

      ])
    | SInclude _ -> t
    | SLet(t,e1,e2) -> SLet(t, cnve e1, cnve e2)
    | SStruct(v,tts) ->
      SStruct(v,
        List.map begin fun(ty,t) ->
          (ty, cnv t)
        end tts)
    | SExp(e) -> SExp(cnve e)
    | SCon(tyss,es,t1) -> t
    | SBlock(l) -> SBlock(List.map cnv l)
    | SEmpty -> t
    | SIf(e1,t1,t2) -> SIf(cnve e1, cnv t1, cnv t2)
    | SRet(e) -> SRet(cnve e)
    | SFun(ty, id, tyis, t2) -> SFun(ty, id, tyis, cnv t2)

  and print (t:t):string = 
    let rec print sp e =
    match e with
    | SBlock ls -> Printf.sprintf "{\n%s\n%s}" (print_ls ~sep:"\n" (print (sp^"  "))ls) sp
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
    | SStruct (id, ts) ->
      Printf.sprintf "%sstruct %s{\n%s\n%s};\n"
        sp
        id
        sp
        (List.fold_left (print_mem sp) "" ts)
    | SList ls -> List.fold_left(fun r t -> r ^ (print sp t) ^ "\n") "" ls  
    | SEmpty -> ""
    | SExp e -> Exp.print sp e ^ ";"
    | SRet e -> sp ^ "return " ^ Exp.print "" e ^ ";"
    | SInclude s -> "#include " ^ s
    | SLet (t, id, Exp.EEmpty) -> Printf.sprintf "%s%s %s" sp (Ty.print "" t) (Exp.print "" id) ^ ";"
    | SLet (t, id, e) -> Printf.sprintf "%s%s %s = %s" sp (Ty.print "" t) (Exp.print "" id) (Exp.print "" e) ^ ";"
    | SCon(tis,[],e) ->
      Printf.sprintf "(%s)%s"
        (print_ls (fun(t,i)->Ty.print "" t ^ " "^i) tis ~sep:", ") (print ("  "^sp) e)
    | SCon(tis,es,e) ->
      Printf.sprintf "(%s):%s%s"
        (print_ls (fun(t,i)->Ty.print "" t ^ " "^i) tis ~sep:", ")
        (print_ls (Exp.print "") es ~sep:", ") (print ("  "^ sp) e)
    | _ -> assert false
    and print2 sp e ed = 
      match e with
      | SBlock ls -> " " ^ print sp e ^ (if ed <> "" then " " else "")
      | _ -> "\n" ^ print (sp^"  ") e ^ ed
    and print_mem sp r (t,s) =
      (match t,s with
      | Ty.TFun(_,_),SExp(s) ->
        Printf.sprintf "%s%s;\n"
          r
          (Ty.print ("  "^sp) t ~pp:(Exp.print "" s))
      | _ ->
        Printf.sprintf "%s%s %s\n"
          r
          (Ty.print ("  "^sp) t)
          (print "" s)
    )
    in print "" t

  let prints (ts:t list): string =
    print_ls ~sep:"\n" print ts

end

open Exp
open Stmt
open Ty
let _ =
  let e = SList [

    (*#include "fib2.h"*)
    SInclude "\"fib2.h\"";

    (* int Int_classId = Class_genId(); *)
    SLet(Ty "int", EVar "Int_classId", ECall(EVar"Class_genId",[]));

    (*
      struct Int {
        int id;
        int x;
        Int(int x):id(Int_classId),x(x){}
      };
    *)
    SStruct ("Int",[
      Ty "int", SExp(EVar "id");
      Ty "int", SExp(EVar "x");
      Ty "Int", SCon([Ty "int","x"],
        [
          ECall(EVar "id",[EVar "Int_classId"]);
          ECall(EVar "x",[EVar "x"])
        ], SBlock([]))
    ]);

    STrait("Fib",[
      TFun (Ty "int", [Ty "Class*"]), SExp(EVar "fib")
    ]);

    SImpl("Fib","Int",[
      SFun(Ty "int", "fib",[],SBlock[
        SIf(EBin(EBin(EVar "self", "->", EVar "x"),"<", EInt 2), SRet(EInt 1), SEmpty);

        SLet(Ty "Int", ECall(EVar "p1",[EBin(EBin(EVar "self", "->", EVar "x"),"-", EInt 2)]), EEmpty);
        SLet(Ty "Int", ECall(EVar "p2",[EBin(EBin(EVar "self", "->", EVar "x"),"-", EInt 1)]), EEmpty);

        SRet(EBin(
          ECallM("Fib", EBin (EVar "p1", "->", EVar "fib"), []),
          "+",
          ECallM("Fib", EBin (EVar "p2", "->", EVar "fib"), [])
        ))
      ])
    ]);

    SFun(Ty "int", "main",[],SBlock[
      SLet(Ty "long",EVar "start", ECall(EVar "gett",[]));
      SLet(Ty "Int", ECall(EVar "p",[EInt 40]), EEmpty);
      SExp(ECall(EVar "printf",[EString "\"%d\\n\"";
        ECallM("Fib", EBin (EVar "p", "->", EVar "fib"), [])

      ]));

      SExp(ECall(EVar "printf",[EString "\"%ld\\n\""; EBin(ECall(EVar "gett",[]),"-",EVar "start")]));
      SRet(EInt 0)
    ]);

  ] in
  Printf.printf "%s\n" (Stmt.print (cnv e));
