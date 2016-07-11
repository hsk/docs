Require Import ExtrOcamlNatInt.

Definition plus (n : nat)(m : nat) : nat :=
  let n1 := n in
  let m1 := m in
  let r := n1 + m1 in
  r.

Extraction "let.ml" plus.

