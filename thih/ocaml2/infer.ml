(* 11 Type Inference *)
(*|
# Infer

    >>> open Infer;;
    
*)

open Pred
open Assump
open TIMonad

type ('e, 't) infer = ti -> classEnv -> assump list -> 'e -> pred list * 't

(*|

*)
