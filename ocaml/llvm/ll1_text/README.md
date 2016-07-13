# ll1_text

ll1_textは文字列をパースして、ASTを作成し、ASTをコンパイルして仮装LLVM命令のASTに変換した後、ファイルにテキストファイルとして出力し、LLVMのllcコマンドを使って四則演算を行うプログラムです。

字句解析:

```
whitespace ::= ' ' | '\t'
eol        ::= '\n'
i          ::= ('0'|...|'9')+
keywords   ::= '+' | '-' | '*' | '/' | '(' | ')'
```

構文:

```
main::=                 メイン
        expr eol        式と改行

expr::=                 式
        i               整数
      | "(" expr ")"    かっこ
      | expr "+" expr   足し算
      | expr "-" expr   引き算
      | expr "*" expr   掛け算
      | expr "/" expr   割り算
      | "-" expr        マイナス

t   ::=                 型
        void            void型
      | i64             64bit int型

x                       文字列
r   ::=                 レジスタ
        rl t x          ローカル変数
      | rn t x          即値

v   ::=                 仮想LLVM命令
        r1 = add r2 r3  足し算
      | r1 = sub r2 r3  引き算
      | r1 = mul r2 r3  掛け算
      | r1 = div r2 r3  割り算
      | print r
```

## コンパイラのパス

- 字句解析 lexer.mll
- 構文解析 parser.mly
- コンパイル main.ml
- 出力 main.ml (Emitモジュール)
