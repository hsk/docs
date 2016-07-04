# calc_ast

calc_astはflex, bisonを使って構文木を作成し、構文木の内容を表示します。

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
