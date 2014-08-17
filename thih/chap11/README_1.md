## 11.1  Literals

Like other languages, Haskell provides special syntax for constant values of certain primitive datatypes, including numerics, characters, and strings.

We will represent these literal expressions as values of the Literal datatype:

	  data Literal = LitInt  Integer
	               | LitChar Char
	               | LitRat  Rational
	               | LitStr  String

Type inference for literals is straightforward.

For characters, we just return tChar.

For integers, we return a new type variable v together with a predicate to indicate that v must be an instance of the Num class.

The cases for String and floating point literals follow similar patterns:

	  tiLit            :: Literal -> TI ([Pred],Type)
	  tiLit (LitChar _) = return ([], tChar)
	  tiLit (LitInt _)  = do v <- newTVar Star
	                         return ([IsIn "Num" v], v)
	  tiLit (LitStr _)  = return ([], tString)
	  tiLit (LitRat _)  = do v <- newTVar Star
	                         return ([IsIn "Fractional" v], v)

