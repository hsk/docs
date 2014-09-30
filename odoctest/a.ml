(*|

# a.ml

	>>> open A;;
	
*)

open B

(*|

## type e

### 1

	>>> V 1;;
	- : A.e = V 1

### 1 + 2

	>>> (Add ((V 1),(V 2)));;
	- : A.e = Add (V 1, V 2)

### 1 - 2

	>>> Sub (V 1, V 2);;
	- : A.e = Sub (V 1, V 2)

*)
type e =
  | V of int
  | Add of e * e
  | Sub of e * e

(*|

# eval

## eval 1

	>>> eval (V 1);;
	- : int = 1

## eval 1 + 2

	>>> eval (Add ((V 1),(V 2)));;
	- : int = 3

## eval 1 - 2

	>>> eval (Sub (V 1, V 2));;
	- : int = -1

*)

let rec eval = function
  | V i -> i
  | Add(a,b) -> eval a + eval b
  | Sub(a,b) -> eval a - eval b

let nn = B.n
