{- |
# Ls Module
-}

import Control.Monad
import Test.HUnit

data Ls a = Ls [a] deriving (Show, Eq)

{- |
## Monad Ls

Lsのテスト

    >>> Ls [1,2]
    Ls [1,2]

    >>> Ls[1,2] >>= \x -> Ls [x*10]
    Ls [10,20]

    >>> Ls[1,2] >>= \x -> return (x*10)
    Ls [10,20]

    >>> :{
        do
           let ls = [1,2,3]
           x <- Ls ls
           return (x * 10)
    :}
    Ls [10,20,30]

    >>> (Ls[1,2] >>= \x -> Ls [x*10]) >>= \x -> Ls [x / 10] 
    Ls [1.0,2.0]

    >>> Ls[1,2] >>= (\x -> Ls [x*10] >>= \x -> Ls [x / 10])
    Ls [1.0,2.0]

    >>> (Ls[1,2] >>= \x -> Ls [x+10]) >>= \x -> Ls [x / 10] 
    Ls [1.1,1.2]
    
    >>> Ls[1,2] >>= (\x -> Ls [x+10] >>= \x -> Ls [x / 10])
    Ls [1.1,1.2]


Listのテスト

    >>> [1,2] >>= \x -> [x*10]
    [10,20]

    >>> [1,2] >>= \x -> return (x*10)
    [10,20]
    
    >>> :{
        do
                   let ls = [1,2,3]
                   x <- ls
                   return (x * 10)
    :}
    [10,20,30]

    >>> ([1,2] >>= \x -> [x*10]) >>= \x -> [x / 10] 
    [1.0,2.0]

    >>> [1,2] >>= (\x -> return(x*10) >>= \x -> return(x / 10))
    [1.0,2.0]

-}
instance  Monad Ls where
    (Ls m) >>= k     =
      Ls $ concat (map (\a -> case k a of (Ls a) -> a) m)
    return x         = Ls [x]
    fail s           = Ls []


main::IO ()
main = do
  runTestTT $ TestList
    [ 
      "Ls 1" ~: do
        let xs = Ls [1,2]
        xs @?= Ls [1, 2]
      ,
      "Ls 2" ~: do
        let xs = Ls[1,2] >>= \x -> Ls [x*10]
        xs @?= Ls [10, 20]
      ,
      "Ls 3" ~: do
        let xs = Ls[1,2] >>= \x -> return (x*10)
        xs @?= Ls [10, 20]
      ,
      "Ls 4" ~: do
        let ls = [1,2,3]
        let xs = do
                   x <- Ls ls
                   return (x * 10)
        xs @?= Ls [10, 20, 30]
      ,
      "Ls 5" ~: do
        let xs = (Ls[1,2] >>= \x -> Ls [x*10]) >>= \x -> Ls [x / 10] 
        xs @?= Ls [1.0, 2.0]
        let xs = Ls[1,2] >>= (\x -> Ls [x*10] >>= \x -> Ls [x / 10])
        xs @?= Ls [1.0, 2.0]
      ,
      "Ls 6" ~: do
        let xs = (Ls[1,2] >>= \x -> Ls [x+10]) >>= \x -> Ls [x / 10] 
        xs @?= Ls [1.1, 1.2]
        let xs = Ls[1,2] >>= (\x -> Ls [x+10] >>= \x -> Ls [x / 10])
        xs @?= Ls [1.1, 1.2]
      ,

      "List 1" ~: do
        let xs = [1,2] >>= \x -> [x*10]
        xs @?= [10, 20]
      ,
      "List 3" ~: do
        let xs = [1,2] >>= \x -> return (x*10)
        xs @?= [10, 20]
      ,
      "List 4" ~: do
        let ls = [1,2,3]
        let xs = do
                   x <- ls
                   return (x * 10)
        xs @?= [10, 20, 30]
      ,
      "List 5" ~: do
        let xs = ([1,2] >>= \x -> [x*10]) >>= \x -> [x / 10] 
        xs @?= [1.0, 2.0]
        let xs = [1,2] >>= (\x -> return(x*10) >>= \x -> return(x / 10))
        xs @?= [1.0, 2.0]

    ]
  return ()
