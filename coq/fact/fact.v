Require Import ExtrOcamlNatInt.

Fixpoint fact (n : nat) :=
  match n with
  | O => 1
  | S m => n * fact m
  end.


Extraction "fact.ml" fact.

