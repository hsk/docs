# a.ml

	>>> open A;;
	


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

