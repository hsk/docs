Require Import ExtrOcamlNatInt.
Require Import ExtrOcamlString.
Require Export Arith.EqNat.
Require Export Bool.Bool.
Require Import String.
Open Scope string_scope.

Inductive id : Type :=
  Id : nat -> id.

Inductive tm : Type :=
  | tm_var : id -> tm
  | tm_app : tm -> tm -> tm
  | tm_abs : id -> tm -> tm
  | tm_true : tm
  | tm_false : tm
  | tm_if : tm -> tm -> tm -> tm.

Definition beq_id (id1:id) (id2:id) :bool :=
  match (id1, id2) with
  | (Id n1, Id n2) => beq_nat n1 n2
  end.

Fixpoint beq_tm (tm1:tm) (tm2:tm) :bool :=
  match (tm1, tm2) with
  | (tm_var id1, tm_var id2) => beq_id id1 id2
  | (tm_app x1 y1, tm_app x2 y2) => beq_tm x1 x2 && beq_tm y1 y2
  | (tm_true, tm_true) => true
  | (tm_false, tm_false) => true
  | (tm_if x1 y1 z1, tm_if x2 y2 z2) => beq_tm x1 x2 && beq_tm y1 y2 && beq_tm z1 z2
  | (_, _) => false
  end.

Fixpoint subst (s:tm) (x:id) (t:tm) : tm :=
  match t with
  | tm_var x' => if beq_id x x' then s else t
  | tm_abs x' t1 => tm_abs x' (if beq_id x x' then t1 else (subst s x t1))
  | tm_app t1 t2 => tm_app (subst s x t1) (subst s x t2)
  | tm_true => tm_true
  | tm_false => tm_false
  | tm_if t1 t2 t3 => tm_if (subst s x t1) (subst s x t2) (subst s x t3)
  end.

Fixpoint eval (tm0:tm) : tm :=
  match tm0 with
  | tm_var x => tm_var x
  | tm_true => tm_true
  | tm_false => tm_false
  | tm_if e1 e2 e3 => if beq_tm (eval e1) tm_true then eval e2 else eval e3
  | tm_abs x e => tm_abs x e
  | tm_app e1 e2 =>
      match eval e1 with
      | tm_abs x e => subst e x (eval e2)
      | e1' => tm_app e1' e2
      end
  end.

Extraction "lambda.ml" eval.
