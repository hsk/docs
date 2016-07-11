Require Import ExtrOcamlNatInt.

Inductive e : Type :=
  | EInt : nat -> e
  | EAdd : e -> e -> e
  | ESub : e -> e -> e
  | EMul : e -> e -> e.

Fixpoint eval (e : e) : nat :=
  match e with
  | EInt n => n
  | EAdd a b => (eval a) + (eval b)
  | ESub a b => (eval a) - (eval b)
  | EMul a b => (eval a) * (eval b)
  end.

Extraction "calc.ml" eval.
