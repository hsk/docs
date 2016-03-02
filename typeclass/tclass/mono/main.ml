
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

let union l1 l2 =
  List.fold_right (fun s l ->
    if List.mem s l then l else s::l
  ) l1 l2

let rec ftv_type(t: t): string list =
  match t with
  | TVar(n)      -> [n]
  | TInt         -> []
  | TBool        -> []
  | TFun(t1, t2) -> union (ftv_type t1) (ftv_type t2)

let subst = ref []

let rec apply_type (t: t): t =
  match t with
  | TVar(n)      -> if List.mem_assoc n !subst then List.assoc n !subst else TVar n
  | TFun(t1, t2) -> TFun(apply_type t1, apply_type t2)
  | t            -> t

let rec apply_assumps (assumps: assumps): assumps =
  List.map (fun (k, v) ->
    (k, apply_type v)
  ) assumps

let nullSubst = []


let count = ref 0

let rec newTVar(prefix: string): t =
  let s = !count in
  count := s + 1;
  TVar(prefix ^ string_of_int s)

let rec varBind(u: string) (t: t) = 
  if t = TVar(u) then () else
  if List.mem u (ftv_type t) then
    raise (TypeError("occurs check fails: " ^ u ^ " vs. " ^ show_t t))
  else subst := (u, t) :: !subst

let rec mgu(t1: t) (t2: t): unit =
  match (t1, t2) with
  | (TFun(l, r),TFun(l2, r2)) ->
    mgu l l2;
    mgu (apply_type r) (apply_type r2)
  | (TVar(u), t) -> varBind u t
  | (t, TVar(u)) -> varBind u t
  | (TInt, TInt) -> ()
  | (TBool,TBool) -> ()
  | (t1,t2) ->
    raise (TypeError("types do not unify: " ^ show_t t1 ^ " vs. " ^ show_t t2))

(* Main type inference function *)
let rec ti(env: assumps) (e: e): t =
  match e with
  | EVar(n) ->
    if List.mem_assoc n env then
      List.assoc n env
    else
      raise(TypeError("unbound variable: " ^ n))
  | EInt(_)  -> TInt
  | EBool(_) -> TBool
  | EAbs(n, e) ->
    let tv = newTVar "'a" in
    let env2 = (n , tv) :: env in
    let t1 = ti env2 e in
    TFun(apply_type tv, t1)
  | EApp(e1, e2) ->
    begin try
      let tv = newTVar "'a" in
      let t1 = ti env e1 in
      let t2 = ti (apply_assumps env) e2 in
      mgu (apply_type t1) (TFun(t2, tv));
      apply_type tv
    with
      | TypeError(msg) -> raise (TypeError (msg ^ "\n in " ^ show_e e))
    end
  | ELet(x, e1, e2) ->
    let t1 = ti env e1 in
    let env2 = (x, t1) :: env in
    let t2 = ti (apply_assumps env2) e2 in
    t2

let rec type_inference(env:assumps) (e: e):t = 
  subst := [];
  let t = ti env e in
  apply_type t

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

