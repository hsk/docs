# MinCamlAsm

このプロジェクトは、MinCamlのX86用のアセンブラ変換部分のみを取り出し、FloatをなくしてC++に移植したものです。

## ビルド

	make

## 使い方

	./calc x86_test/a

## ライセンス

MIT License

## ファイル構成

1. __Makefile__ valgrindでメモリリークチェック
2. __ast.h__ 抽象構文木の定義と、グローバルな関数定義
3. __ast.cpp__ 自由変数の計算など
4. __ast_print.cpp__ 抽象構文木のプリティプリント
5. __parser.y__ 構文解析器
6. __lexer.l__ 字句解析
7. __simm.cpp__ 即値最適化
8. __regAlloc.cpp__ レジスタアロケーション
9. __emit.cpp__ x86アセンブラ出力
10. __stub.c__ 出力アセンブラとリンクするx86のメイン関数
11. __x86_libmincaml.s__ 出力アセンブラとリンクするライブラリ

## MinCamlAsm構文

### 字句解析構文

    digit     ::= '0'|...|'9'
    space     ::= ' '|'\t'|'\n'|'\r'
    lower     ::= 'a'|...|'z'
    upper     ::= 'A'|...|'Z'
    spaces    ::= space+
    INT       ::= digit+
    IDENT     ::= (upper|lower|'_'|'%')(digit|lower|upper|'_')* 

### 文脈自由文法

    program   ::= fundefs term
    fundefs   ::= /* empty */
                | fundefs fundef
    fundef    ::= "define" ident "(" args ")" ":" ty "{" term "}"
    args      ::= /* empty */
                | id_ty
                | args "," id_ty
    id_ty     ::= ident ":" ty
    ty        ::= "unit"
                | "bool"
                | "int"
                | "(" tys ")" "->" ty
    tys       ::= /* empty */
                | tys "," ty
    id_or_imm ::= ident
                | INT
    term      ::= exp
                | ident ":" ty "=" exp term
                | exp term
    exp       ::= "nop"
                | "set" INT
                | "setl" ident
                | "mov" ident
                | "neg" ident
                | "add" ident id_or_imm
                | "sub" ident id_or_imm
                | "ld" ident id_or_imm INT
                | "st" ident ident id_or_imm INT
                | "ifeq" ident id_or_imm "{" term "}" "else" "{" term "}"
                | "ifle" ident id_or_imm "{" term "}" "else" "{" term "}"
                | "ifge" ident id_or_imm "{" term "}" "else" "{" term "}"
                | "call" ident "(" args ")"
                | "save" ident ident
                | "restore" ident
    ident     ::= IDENT

## 説明

MinCamlAsmは、パースした構文木をsimmで即値最適化をした後、regAllocでレジスタアロケーションを行い、emitでx86のアセンブラを生成します。

1. parse.y lexer.l 構文解析
2. simm.cpp 即値最適化
3. regAlloc.cpp レジスタアロケーション
4. emit.cpp x86アセンブラ出力

## 構文解析 parser.y lexer.l

構文解析は、flex, bisonを用いています。
そのため左再帰も右再帰も扱うことができるので便利です。
ASTを引き渡す際に、unionを用いるため、値をポインタで持つ必要がある点に注意が必要です。
ASTにはunique_ptrを使っているので、うまくメモリリークしないような工夫をしました。

## 即値最適化 simm

## レジスタアロケーション regAlloc

## x86アセンブラ出力 emit
