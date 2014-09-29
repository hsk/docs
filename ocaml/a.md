

# a.ml

とりあえずopen

	>>> open A;;

## eval 1

	>>> eval (V 1);;
	- : int = 1

## eval 1 + 2

	>>> eval (Add ((V 1),(V 2)));;
        - : int = 3

## 1 + 2

	>>> (Add ((V 1),(V 2)));;
	- : A.e = Add (V 1, V 2) 

## 1 - 2

	>>> Sub (V 1, V 2);;
	- : A.e = Sub (V 1, V 2) 

## eval 1 - 2

	>>> eval (Sub (V 1, V 2));;
	- : int = -1


