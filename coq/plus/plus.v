Require Import ExtrOcamlNatInt.

Definition plus (n : nat)(m : nat) : nat :=
  n + m.


Extraction "plus.ml" plus.

