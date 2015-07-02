# ATS パーサコンビネータ

パーサコンビネータを作ってみました。
実行するとファイルを読み込んで構文木と、実行結果を表示します。


### ビルド

```
$ make
```

### a.txt

```
/* test programming language */
let a = 10 in
let b = 100 in
let c =
  let a = 2 in
  let b = 1 in
  a - b
in
let d = a + b + c in
d
```

### 実行

```
$ ./main a.txt
```

### 結果

```
AST=Let( "a",Int(10), Let( "b",Int(100), Let( "c",Let( "a",Int(2), Let( "b",Int(1), Bin(Var("a"), "-", Var("b")))), Let( "d",Bin(Bin(Var("a"), "+", Var("b")), "+", Var("c")), Var("d")))))

/* test programming language */
let a = 10 in
let b = 100 in
let c =
  let a = 2 in
  let b = 1 in
  a - b
in
let d = a + b + c in
d=111
```

### クリーン

```
$ make clean
```

