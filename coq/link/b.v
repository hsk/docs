Require Import ExtrOcamlNatInt.
Require Import a.

Definition b n m := max n m.

Extraction "b.ml" b.
