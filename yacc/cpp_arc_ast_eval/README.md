# cpp\_arc\_ast\_eval

cpp\_arc\_ast\_evalはflex, bisonを使って構文木を作成し、構文木を計算します。
c言語から、c++への変更は、出力ファイル名の変更を調整すれば動作するようです。

cpp\_arc\_ast\_evalではメモリ管理をObjective-Cのオートリリースプールのようにして管理してます。
arc.hppのマクロarc_instanceを実行すると必要なグローバル変数が用意されます。
arc_instanceをメインプログラムに書いておくと必要なグローバル変数が用意されます。
arc.hppは実験的に作ったものですので、様々な環境の動作を保証はできません。

```
struct E : arc::object {};
```

のように、arc::objectを継承することで、オートリリースプールで管理されるオブジェクトが作れます。


## Lexical Syntax

```
space    ::= (' ' | '\t')
DOUBLE   ::= (('1'|...|'9') ('0'|...|'9')* | '0') ('.' ('0'|...|'9') ('0'|...|'9')*)?
paren    ::= '(' | ')'
operator ::= '+' | '-' | '*' | '/'
```

## Operator Precedence

assoc | operators
----- | ---------
left  | '+' '-'
left  | '*' '/'

## Context-free Syntax

```
program       ::= expr
expr          ::= expr '+' expr
                | expr '-' expr
                | expr '*' expr
                | expr '/' expr
                | primary
primary       ::= DOUBLE
                | '(' expr ')'
```
