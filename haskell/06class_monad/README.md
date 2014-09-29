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

# Ls Module
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

