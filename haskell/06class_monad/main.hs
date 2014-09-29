
import Control.Monad

data OkNg a = Ok a | Ng String
instance  Monad OkNg  where
    (Ok x) >>= k   =  k x
    (Ng x) >>= k   =  Ng x
    return           =  Ok
    fail s           =  Ng s

main = do
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
    