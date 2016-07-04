
## Lexical Syntax

```
space    ::= (' ' | '\t')
DOUBLE   ::= (('1'|...|'9') ('0'|...|'9')* | '0') ('.' ('0'|...|'9') ('0'|...|'9')*)?
operator ::= '+' '-' '*' '/'
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
