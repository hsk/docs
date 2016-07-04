
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module E where

import Data.List

data E a = V a
  | Add (E a) (E a)
  deriving (Eq, Show)

eval :: Num a => E a -> a
eval (V a)     = a
eval (Add a b) = eval a + eval b

eval2 :: E [a] -> [a]
eval2 (V a)     = a
eval2 (Add a b) = eval2 a ++ eval2 b


eval3 :: E a -> E a
eval3 (V a) = V a
eval3 (Add a b) = (Add a b)

eval4 :: E Integer -> Integer
eval4 (V a) = a
eval4 (Add a b) = eval4 a + eval4 b

aa = 1


class K a where
  ev :: a -> Integer
  kk :: a -> Integer

instance K (E Integer) where
  ev (V a)     = a
  ev (Add a b) = ev a + ev b
  kk a = (ev a) + 1

instance K Integer where
  ev a = a
  kk a = a + 1
