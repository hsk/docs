# Monad Ok

test1

    >>> return "a" >>= \a -> return (a ++ "b") >> fail "error" >> return "2"
    *** Exception: user error (error)

test2

    >>> (Ok "a") >>= \a -> Ok (a ++ "b") >>= \ab -> Ok (ab++"2")
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

