# MLのプリティプリント

```
type e =
  | Int of int
  | Let of string * e * e
  | Var of string
```

のプリティプリントについて考えます。

出来たらここに、コメントを追加して奇麗にプリティプリントする事を考えてみます。

## 1. 簡単なプリティプリント

```
let rec pp sp = function
  | Int(i) ->
    Printf.sprintf "%s%d\n" sp i
  | Var(x) ->
    Printf.sprintf "%s%s\n" sp x
  | Let(x,e1,e2) ->
    Printf.sprintf "%slet %s =\n%s%sin\n%s"
      sp
      x
      (pp (sp ^ "  ") e1)
      sp
      (pp sp e2)

let _ =
  let e = Let("a",Let("b", Int 1, Var "b"),Let("c", Int 1, Var "b")) in
  let s = pp "" e in
  Printf.printf "%s\n" s
```

結果

```
let a =
  let b =
    1
  in
  b
in
let c =
  1
in
b
```

なんか出来ました。簡単ですね。

## 2. コメントつく事を考える。

これが結構問題です。
どう問題なのかというと、

```
(*1*)let(*2*)a(*3*)=(*4*)b(*5*)in(*6*)c(*7*)
```

```
type e =
  | Int of int
  | Let of string * e * e
  | Var of string
  | Comment of string * e * int
```


