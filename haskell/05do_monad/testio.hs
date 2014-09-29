a = do
  putStrLn "a"

b = putStrLn "b"

c = do
  putStrLn "c"
  putStrLn "c2"

d = putStrLn "d" >> putStrLn "d2"

e = let e = "e" in putStrLn e >> putStrLn e
e2 = do
  let e = "e"
  putStrLn e
  putStrLn e

f = do
  return "f"

g = do
  v <- f
  putStrLn v 

h = f >>= putStrLn

i = f >>= \v -> putStrLn v >> return "i" >>= \w -> putStrLn v >> putStrLn w

j = return "j" >>= \j -> putStrLn j

k =
  putStrLn $
    do
      v <- return "k"
      (v++"2")

main = do
  a
  b
  c
  d
  e
  e2
  g
  h
  i
  j
  k

