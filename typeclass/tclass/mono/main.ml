
type e =
  | EVar of string
  | EInt of int
  | EBool of bool
  | EApp of e * e
  | EAbs of string * e
  | ELet of string * e * e

type t =
  | TInt
  | TBool
  | TVar of string
  | TFun of t * t

type subst = (string * t) list

type assumps = (string * t) list

exception TypeError of string

let rec show_t (t: t): string =
  match t with
  | TInt -> "TInt"
  | TBool -> "TBool"
  | TVar(a) -> "TVar(" ^ a ^ ")"
  | TFun(t1,t2) -> "TFun(" ^ show_t t1 ^ ", " ^ show_t t2 ^ ")"

let rec show_e (e: e): string =
  match e with
  | EVar(s) -> "EVar(" ^ s ^ ")"
  | EInt(i) -> "EInt(" ^ string_of_int i ^ ")"
  | EBool(b) -> "EBool(" ^ string_of_bool b ^ ")"
  | EApp(e1,e2) -> "EApp(" ^ show_e e1 ^ ", " ^ show_e e2 ^ ")"
  | EAbs(s, e) -> "EAbs(" ^ s ^ ", " ^ show_e e ^ ")"
  | ELet(s, e1, e2) -> "ELet(" ^ s ^ ", " ^ show_e e1 ^ ", " ^ show_e e2 ^ ")"

let union l1 l2 = l1 @ l2

let rec ftv_type(t: t): string list =
  match t with
  | TVar(n)      -> [n]
  | TInt         -> []
  | TBool        -> []
  | TFun(t1, t2) -> union (ftv_type t1) (ftv_type t2)

let rec apply_type (s: subst) (t: t): t =
  match t with
  | TVar(n)      -> if List.mem_assoc n s then List.assoc n s else TVar(n)
  | TFun(t1, t2) -> TFun(apply_type s t1, apply_type s t2)
  | t            -> t

let rec apply_assumps(s: subst) (assumps: assumps): assumps =
  List.map (fun (k, v) ->
    (k, apply_type s v)
  ) assumps

let nullSubst = []

let subst = ref []

let count = ref 0

let rec newTVar(prefix: string): t =
  let s = !count in
  count := s + 1;
  TVar(prefix ^ string_of_int s)

let rec varBind(u: string) (t: t): subst = 
  if t = TVar(u)
  then nullSubst
  else if List.mem u (ftv_type t) then
    raise (TypeError("occurs check fails: " ^ u ^ " vs. " ^ show_t t))
  else [(u, t)]

let composeSubst(s1: subst) (s2: subst): subst =
  List.map (fun (x, v) ->
      (x, apply_type s1 v)
  ) s2 @ s1

let rec mgu(t1: t) (t2: t): subst =
  match (t1, t2) with
  | (TFun(l, r),TFun(l2, r2)) ->
    let s1 = mgu l l2 in
    let s2 = mgu (apply_type s1 r) (apply_type s1 r2) in
    composeSubst s1 s2
  | (TVar(u), t) -> varBind u t
  | (t, TVar(u)) -> varBind u t
  | (TInt, TInt) -> nullSubst
  | (TBool,TBool) -> nullSubst
  | (t1,t2) ->
    raise (TypeError("types do not unify: " ^ show_t t1 ^ " vs. " ^ show_t t2))

(* Main type inference function *)
let rec ti(env: assumps) (e: e): (subst * t) =
  match e with
  | EVar(n) ->
    if List.mem_assoc n env then
      (nullSubst, List.assoc n env)
    else
      raise(TypeError("unbound variable: " ^ n))
  | EInt(_)  -> (nullSubst, TInt)
  | EBool(_) -> (nullSubst, TBool)
  | EAbs(n, e) ->
    let tv = newTVar("'a") in
    let env2 = (n , tv) :: env in
    let (s1, t1) = ti env2 e in
    (s1, TFun(apply_type s1 tv, t1))
  | EApp(e1, e2) ->
    begin try
      let tv = newTVar("'a") in
      let (s1, t1) = ti env e1 in
      let (s2, t2) = ti (apply_assumps s1 env) e2 in
      let s3 = mgu (apply_type s2 t1) (TFun(t2, tv)) in
      (composeSubst s3 (composeSubst s2 s1), apply_type s3 tv)
    with
      | TypeError(msg) -> raise (TypeError (msg ^ "\n in " ^ show_e e))
    end
  | ELet(x, e1, e2) ->
    let (s1, t1) = ti env e1 in
    let env2 = (x, t1) :: env in
    let (s2, t2) = ti (apply_assumps s1 env2) e2 in
    (composeSubst s1 s2, t2)

let rec type_inference(env:assumps) (e: e):t = 
  let (s, t) = ti env e in
  apply_type s t

let rec test((e: e), (et: t)):unit =
  try
    let t = type_inference [] e in
    if t = et then ()
    else begin
      Printf.printf "%s\n" (show_e e ^ " :: " ^ show_t t ^ "\n");
      assert false
    end
  with
    | TypeError(err) ->
      Printf.printf "%s\n" (show_e e ^ "\n " ^ err ^ "\n");
      assert false


let rec testError(e:e):unit =
  try
    let t = type_inference [] e in
    Printf.printf "%s\n" (show_e e ^ " :: " ^ show_t t ^ "\n");
    assert false
  with
    | TypeError(err) -> ()

let () =
  test(ELet("id", EAbs("x", EVar("x")),
    EVar("id")),
    TFun(TVar("'a0"), TVar("'a0")));

  test(ELet("id", EAbs("x", EVar("x")),
    EApp(EVar("id"), EInt(1))),
    TInt);

  test(ELet("id", EAbs("x", ELet("y", EVar("x"), EVar("y"))),
   EApp(EVar("id"), EInt(1))),
    TInt);

  testError(ELet("id", EAbs("x", ELet("y", EVar("x"), EVar("y"))),
   EApp(EApp(EVar("id"), EVar("id")), EInt(2))));

  testError(ELet("id", EAbs("x", EApp(EVar("x"), EVar("x"))),
   EVar("id")));

  test(EAbs("m", ELet("y", EVar("m"),
        ELet("x", EApp(EVar("y"), EBool(true)),
              EVar("x")))),
    TFun(TFun(TBool,TVar("'a11")),TVar("'a11")));

  testError(EApp(EInt(2), EInt(2)));

