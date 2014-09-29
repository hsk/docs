import Control.Monad
import Test.HUnit

data Ok a = Ok a | Ng String deriving(Eq,Show)


{- |

# Monad Ok

test1

    >>> return "a" >>= \a -> return (a ++ "b") >> fail "error" >> return "2"
    *** Exception: user error (error)

test2

    >>> Ok "a" >>= \a -> Ok (a ++ "b") >>= \ab -> Ok (ab++"2")
    Ok "ab2"

test3

    >>> :{
                do
                  a <- Ok "a"
                  ab <- Ok (a ++ "b")
                  Ng "error2"
                  Ok (ab++"2")
    :}
    Ng "error2"

test4

    >>> :{
                do
                  a <- return "a"
                  ab <- return (a ++ "b")
                  fail "error3"
                  return (ab++"2")
    :}
    *** Exception: user error (error3)

test5

    >>> (f >>= \k -> k)
    "ab"

-}

instance  Monad Ok  where
    (Ok x) >>= k   =  k x
    (Ng x) >>= k   =  Ng x
    return           =  Ok
    fail s           =  Ng s


main::IO ()
main = do
  main2
  runTestTT $ TestList
    [ 
      "Ok 1" ~:
        case return "a" >>= \a -> return (a ++ "b") >> fail "error" >> return "2" of
          (Ok k) -> 1 @=? 2
          (Ng s) -> s @=? "error"
      ,
      "OK 2" ~: do
        let k = Ok "a" >>= \a -> Ok (a ++ "b") >>= \ab -> Ok (ab++"2")
        case k of
          (Ok k) -> "ab2" @=? k
          (Ng s) -> 1 @=? 2
      ,
      "Ok 3" ~: do
        let k =
                do
                  a <- Ok "a"
                  ab <- Ok (a ++ "b")
                  Ng "error2"
                  Ok (ab++"2")
        case k of
          (Ok k) -> 1 @=? 2
          (Ng s) -> "error2" @=? s
      ,

      "Ok 4" ~: do
        let k =
                do
                  a <- return "a"
                  ab <- return (a ++ "b")
                  fail "error3"
                  return (ab++"2")
        case k of
          (Ok k) -> 1 @=? 2
          (Ng s) -> "error3" @=? s
      ,
      "Ok 5" ~: do
          "ab" @=? (f >>= \k -> k)

    ]
  return ()

main2 = do
  putStrLn "test"
  case return "a" >>= \a -> return (a ++ "b") >> fail "error" >> return "2" of
    (Ok k) -> putStrLn k
    (Ng s) -> putStrLn s
  let k = Ok "a" >>= \a -> Ok (a ++ "b") >>= \ab -> Ok (ab++"2")
  case k of
    (Ok k) -> putStrLn k
    (Ng s) -> putStrLn s

  let k =
          do
            a <- Ok "a"
            ab <- Ok (a ++ "b")
            Ng "error2"
            Ok (ab++"2")
  case k of
    (Ok k) -> putStrLn k
    (Ng s) -> putStrLn s

  let k =
          do
            a <- return "a"
            ab <- return (a ++ "b")
            fail "error3"
            return (ab++"2")
  case k of
    (Ok k) -> putStrLn k
    (Ng s) -> putStrLn s

  putStrLn (f >>= \k -> k)


bb = return "a"

f = do
    b <- bb
    return (b ++ "b")
    
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
