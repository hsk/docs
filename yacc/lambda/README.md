# lambda

lambdaはflex, bisonを使って構文木を作成し、λ計算を行います。
現状、GCしないのでメモリリークします。

## Lexical Syntax

```
digit    ::= '0' |...| '9'
alpha    ::= 'a' |...| 'z' | '_'
space    ::= ' ' | '\t' | '\r' | '\n'
DOUBLE   ::= (('1'|...|'9') digit* | '0') ('.' digit+)?
BOOL     ::= "true" | "false"
paren    ::= '(' | ')'
operator ::= '+' | '-' | '*' | '/' | '=' | "::" | "<=" | "->"
keyword  ::= "let" | "rec" | "in" | "if" | "else" | "true" | "false"
IDENT    ::= alpha (alpha | digit)*
```

空白は、' '、'\t'、'\r'、'\n'です。
数値はdouble値のみを使用できます。
キーワードは"let"、 "rec"、 "in"、 "if"、 "else" "true" "false" です。
識別子はアルファベットとアンダーバーから始まり、アルファベットとアンダーバーと数の0個以上の文字列です。
括弧は'('と')'です。
演算子は、'+'、'-'、'*'、'/'、'='、"::"、"<="、"->"があります。
コメントは今の所ありません。

## Operator Precedence

assoc | operators
----- | ---------
right | "->"
right | "::"
left  | "<="
left  | '+' '-'
left  | '*' '/'

## Context-free Syntax

```
program       ::= expr
expr          ::= primary_expr
                | infix_expr
                | control_expr
infix_expr    ::= expr "::" expr
                | expr "<=" expr
                | expr '+' expr
                | expr '-' expr
                | expr '*' expr
                | expr '/' expr

control_expr  ::= IDENT "->" expr
                | expr expr
                | "let" IDENT '=' expr "in" expr
                | "let" "rec" ID '=' expr "in" expr
                | "if" expr "then" expr "else" expr
primary_expr  ::= DOUBLE
                | BOOL
                | IDENT
                | '(' ')'
                | '(' expr ')'
```

## 仕様詳細

### プログラム

```
program       ::= expr
```

lambdaのプログラムは1つの式から構成されます。

### 式

```
expr          ::= primary_expr
                | infix_expr
                | prefix_expr
                | control_expr
```

式はプライマリ式、中置演算子式、コントロール式から構成されています。

### 前置演算子式

```
prefix_expr   ::= '+' expr
                | '-' expr
```

前置演算子式には、'+'演算子と'-'演算子を用いることができます。

### 中置演算子式

```
infix_expr    ::= expr "::" expr
                | expr "<=" expr
                | expr '+' expr
                | expr '-' expr
                | expr '*' expr
                | expr '/' expr
```

中値演算子式で用いることができる演算子は"::" "<=" '+' '-' '*' '/'の6つです。

演算子の優先順位は以下の通りです。表の上が優先順位が低く、下が優先順位が高くなります。

assoc | operators
----- | ---------
right | "->"
right | "::"
left  | "<="
left  | '+' '-'
left  | '*' '/'

### コントロール式

```
control_expr  ::= ID "->" expr
                | expr expr
                | "let" ID '=' expr "in" expr
                | "let" "rec" ID '=' expr "in" expr
                | "if" expr "then" expr "else" expr
```

コントロール式には関数と関数適用、let式、let rec式があります。
関数は引数を1つしか取ることができません。複数の引数を使いたい場合は複数の"->"演算子を使ってカリー化された関数を作成することで実現することが可能です。
例えば、足し算を行う関数は以下のように記述します。

```
let add = x -> y -> x + y in add 1 2
```

"->"演算子は右結合します。
関数呼び出しは式を複数並べて記述します。

'let'式は、変数に束縛します。束縛された値は、変更することができません。

```
let x = 1 in 
let y = 2 in
x + y
```

以下のように、'let'式はネストすることも可能です。

```
let x =
  let x = 1 in 
  let y = 2 in
  x + y
in
let y = 2 in
x + y
```

'let'式は値を上書きはできませんが、新しくスコープを作成するので、名前を上書きすることは可能です。例:

```
let x = 1 in
let x = x + 1 in
x
```

この例では、変数xの名前を上書きしています。

let rec式は、名前を再帰的に呼び出すことが可能にする式です。
以下の例のように、再帰呼び出しをすることができます。

```
let rec sum = x ->
  if x <= 0
  then 0
  else x + sum (x - 1)
in
let x = sum 10 in
x
```

if式は、分岐をすることができます。

### プライマリ式

```
primary_expr  ::= DOUBLE
                | BOOL
                | IDENT
                | '(' ')'
                | '(' expr ')'
```

プライマリ式は倍精度少数点数、BOOL値(trueまたはfalse)、変数、nil、又は括弧で括られた式を書くことができます。
"()" はnil値を表します。
