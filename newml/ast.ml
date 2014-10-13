type t =
  | Ty of string

  | TFun of t * t
  | TTuple of t list
  | TEmpty
  | TGen of t * t list
  | TUnit

type e =
  | EEmpty
  | EInt of int
  | EString of string
  | EVar of string
  | EBin of e * string * e
  | EPre of string * e
  | ECall of e * e list
  | EIf of e * e * e
  | EFun of e list * t * e
  | EPFun of e list
  | EMatch of e * e list
  | EList of e list
  | ELet of string * t * e
  | ELetRec of string * t * e
  | EUnit 
  | EBlock of e list
  | ETypeRec of string * (string * t) list
  | ERecord of (string * e) list
  | ETypeVariant of string * (string * t) list

(*  | ETuple of e list *)

type s = 
  | SOpen of string
  | SLet of string * t * e
  | SLetRec of string * t * e
  | SExp of e
  
type prog =
  | Prog of s list

let (|>) a b = b a

let (<|) f d = d f

let _1 a f = f a

let _2 (a, b) f = f a b

let _3 (a, b, c) f = f a b c

let _4 (a, b, c, d) f = f a b c d

let _5 (a, b, c, d, e) f = f a b c d e

let _6 (a, b, c, d, e, g) f = f a b c d e g
