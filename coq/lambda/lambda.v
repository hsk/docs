Require Import ExtrOcamlNatInt.
Require Import ExtrOcamlString.
Require Export Arith.EqNat.
Require Export Bool.Bool.
Require Import String.
Open Scope string_scope.

Inductive id : Type :=
  Id : nat -> id.

Inductive m : Type :=
  | m_var : id -> m
  | m_app : m -> m -> m
  | m_abs : id -> m -> m
  | m_true : m
  | m_false : m
  | m_if : m -> m -> m -> m.

Definition beq_id (id1:id) (id2:id) :bool :=
  match (id1, id2) with
  | (Id n1, Id n2) => beq_nat n1 n2
  end.

Fixpoint beq_m (m1:m) (m2:m) :bool :=
  match (m1, m2) with
  | (m_var id1, m_var id2) => beq_id id1 id2
  | (m_app x1 y1, m_app x2 y2) => beq_m x1 x2 && beq_m y1 y2
  | (m_true, m_true) => true
  | (m_false, m_false) => true
  | (m_if x1 y1 z1, m_if x2 y2 z2) => beq_m x1 x2 && beq_m y1 y2 && beq_m z1 z2
  | (_, _) => false
  end.

Fixpoint subst (s:m) (x:id) (t:m) : m :=
  match t with
  | m_var x' => if beq_id x x' then s else t
  | m_abs x' t1 => m_abs x' (if beq_id x x' then t1 else (subst s x t1))
  | m_app t1 t2 => m_app (subst s x t1) (subst s x t2)
  | m_true => m_true
  | m_false => m_false
  | m_if t1 t2 t3 => m_if (subst s x t1) (subst s x t2) (subst s x t3)
  end.

Fixpoint eval (m0:m) : m :=
  match m0 with
  | m_var x => m_var x
  | m_true => m_true
  | m_false => m_false
  | m_if e1 e2 e3 => if beq_m (eval e1) m_true then eval e2 else eval e3
  | m_abs x e => m_abs x e
  | m_app e1 e2 =>
      match eval e1 with
      | m_abs x e => subst e x (eval e2)
      | e1' => m_app e1' e2
      end
  end.

Extraction "lambda.ml" eval.
