import Control.Monad
import Test.HUnit

data Ok a = Ok a | Ng String

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
    
