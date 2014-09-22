# Haskellを思い出す

# ghci とソース

以下の作業で、ソースを読み込んでmainを実行してみれる。

```
mkdir test1
cd test1
echo 'main = putStrLn "Hello, World!"' > a.hs
$ ghci a.hs
GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
[1 of 1] Compiling Main             ( a.hs, interpreted )
Ok, modules loaded: Main.
*Main> main
Hello, World!
*Main> :q
Leaving GHCi.
$
```

# ghc

コンパイル

```
$ ghc -o a a.hs
[1 of 1] Compiling Main             ( a.hs, a.o )
Linking a ...
$ a
Hello, World!
```

# モジュール

```
$ cat a.hs
module A where

import B

main = putStrLn (show f)

$ cat b.hs
module B where
f = 1

$ runghc a.hs
1
```

