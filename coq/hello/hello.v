Require Import ExtrOcamlString.

Require Import String.
Open Scope string_scope.

Definition msg := "Hello world!".

Extraction "hello.ml" msg.

