{- |
# B module

-}
module B where
{- |

## E

	>>> Add (V 1) (V 2)
	Add (V 1) (V 2)
-}

data E a
  = Add (E a) (E a)
  | V a
  deriving(Eq,Show)

{- |

## eval

	>>> eval (Add (V 1) (V 2))
	3


-}
eval (V a) = a
eval (Add a b) = eval a + eval b

