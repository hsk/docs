{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where


import E

main = do
  putStrLn (show (Add (V 5) (V 10)))
  let e = (Add (V 5) (V 10)) :: E Integer
  putStrLn (show $ ev e)
