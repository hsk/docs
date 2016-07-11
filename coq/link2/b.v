Require Import ExtrOcamlNatInt.
Require Import abc.a.

Definition b n m := max n m.

Extraction "b.ml" b.
