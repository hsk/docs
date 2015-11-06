open Vm
open Printf

type t =
  | TInt
  | TPair

type e =
  | Int of int
  | Add of e * e
  | Mul of e * e
  | Let of string * e * e
  | Var of string
  | Call of string * e list
type te =
  | TeLet of string * e * te
  | TeLetRec of string * (string * t)list * e * te
  | TeNil

let datas:c list ref = ref []

let genv = ref []

let add c =
  datas := c :: !datas


let rec compe env = function
  | Int i -> add(CPush i); env
  | Add(e1,e2) ->
    let env = compe env e1 in
    let env = compe env e2 in
    add (CAdd);
    env
  | Mul(e1,e2) ->
    let env = compe env e1 in
    let env = compe env e2 in
    add (CMul);
    env
  | Let(x, e1, e2) ->
    let env = compe env e1 in
    let i = List.length env in
    add (CStore i);
    compe ((x,i)::env) e2
  | Var(x) -> add(CLoad (List.assoc x env)); env
  | Call(x, es) ->
    let env = List.fold_left (fun env e ->
      compe env e
    ) env es in
    if List.mem_assoc x !genv then
      let (p,pn,an) = List.assoc x !genv in
      add(CCall(p,pn,an))
    else begin
      let p = -(List.length !genv)-1 in
      genv := !genv @ [x,(p,List.length es,-1)];
      add(CCall(p,List.length es,-1))
    end;
    env

let rec compte = function
  | TeNil -> ()
  | TeLet(x,e,te) -> ()
  | TeLetRec(x, params, e, te) ->
    let fpos = List.length !datas in
    let (prm_len,params) = List.fold_left(fun (i, ls) (x,t) ->
      (i+1, (x,i)::ls)
    ) (0, []) params in
    let params = List.rev params in
    let env = compe params e in
    if List.mem_assoc x !genv then begin
      let (p,_,_) = List.assoc x !genv in
      datas := List.map(function
        | CCall(x,_,_) when x = p ->
          CCall(fpos, prm_len, List.length env)
        | c -> c
      ) !datas
    end
    else
      genv := !genv @ [x,(fpos, prm_len, List.length env)];
    add(CRet);
    compte te

let compile te e =
  datas := [];
  genv := [];
  compe [] e;
  add CHalt;
  compte te;
  Array.of_list (List.rev !datas)

let _ =
  printf "start test compile\n";
  assert (1 = Vm.run(compile TeNil (Int 1) ));
  assert (3 = Vm.run(compile TeNil (Add(Int 1,Int 2)) ));
  assert (1 = Vm.run(compile
    (TeLetRec("main",[], Int 1, TeNil))
    (Call("main",[]))
  ));
  assert (3 = Vm.run(compile
    (TeLetRec("main",[], Add(Int 1, Int 2), TeNil))
    (Call("main",[]))
  ));
  assert (55 = Vm.run(compile
    (TeLetRec("main",["x",TInt], Add(Int 33, Var "x"), TeNil))
    (Call("main",[Int 22]))
  ));
  assert (55 = Vm.run(compile
    (TeLetRec("main",["x",TInt;"y",TInt], Add(Var "y", Var "x"), TeNil))
    (Call("main",[Int 22;Int 33]))
  ));
  assert (33333 = Vm.run(compile
    (TeLetRec("add",["x",TInt;"y",TInt], Add(Var "y", Var "x"),
     TeLetRec("main",["x",TInt;"y",TInt], Call("add", [Var "y"; Var "x"]),
     TeNil)))
    (Call("main",[Int 11111;Int 22222]))
  ));
  printf "end test compile\n";
