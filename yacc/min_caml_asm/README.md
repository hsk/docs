# MinCamlAsm

このプロジェクトは、MinCamlのX86用のアセンブラ変換部分のみを取り出し、浮動小数点数やArray等の型をなくしてC++に移植したものです。

## 依存プログラム

flex, bison, clang, valgrind

    brew install flex bison clang valgrind

## ビルド

    git clone https://github.com/hsk/docs
    cd docs/yacc/min_caml_asm
    make

## 使い方

	./mincamlasm x86_test/fib
    ./x86_test/fib.exe

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
10. __main.cpp__ メインプログラム
11. __stub.c__ 出力アセンブラとリンクするx86のメイン関数
12. __x86_libmincaml.s__ 出力アセンブラとリンクするライブラリ

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

MinCamlAsmはパースした構文木をsimmで即値最適化をした後、regAllocでレジスタアロケーションを行い、emitでx86のアセンブラを生成します。

C++版とOCaml版の大きな違いはshared\_ptrではなくunique\_ptrを用いてメモリの移動を考慮に入れたアルゴリズムにしてある点です。
おそらく、Rustのプログラムに考え方は似ているのではないかと思いますが、データは持ち主が一人いて所有権を移動させながらうまくやりくりしています。
2箇所以上で所有したい場合はディープコピーが必要になるためcloneメソッドを各ASTに実装し用いています。

C++にはパターンマッチがないのでdynamic_castによる型の分岐を用いています。
高速化を考えるとメソッドディスパッチあるいはvisitorパターンで書き換えた方がよいかもしれません。
ここでは、このやり方はOCamlのプログラムに近いという意味でわかりやすさを優先してdynamic_castを用いました。

1. parse.y lexer.l 構文解析
2. simm.cpp 即値最適化
3. regAlloc.cpp レジスタアロケーション
4. emit.cpp x86アセンブラ出力

## 構文解析 parser.y lexer.l

構文解析はflex, bisonを用いています。
そのため左再帰も右再帰も扱うことができるので便利です。
ASTを引き渡す際にunionを用いるため値をポインタで持つ必要がある点に注意が必要です。
ASTにはunique_ptrを使っているので、うまくメモリリークしないような工夫をしました。

## 即値最適化 simm

変数に割り付けられた値がわかっていれば、即値として展開できる箇所は展開することで高速化します。

simm関数がエントリポイントでstatic関数であるwalk\_prog,walk\_fundef,walk\_e,walk\_expが対応するデータ構造に対する処理です。
即値にできるタイミングで環境に変数名(レジスタ名)の値が入っていれば即値に置き換えます。

    // 各命令の即値最適化
    static UExp walk_exp(std::map<std::string,int> env, UExp e)
    // Let,Ansの即値最適化
    static UE walk_e(std::map<std::string,int> env, UE e)
    // トップレベル関数の即値最適化
    static UFundef walk_fundef(UFundef fundef)
    // プログラム全体の即値最適化
    static UProg walk_prog(UProg prog)
    UProg simm(UProg prog)


## レジスタアロケーション regAlloc

無限レジスタ（変数）を有限個の実レジスタに置き換えます。
simm関数と同様にregAllocがエントリポイントでwalk\_prog,walk\_fundef,walk\_e,walk\_expが対応するデータ構造に対する処理です。
fv関数を用いて後続の生きている情報を取得し、環境regenv\_tの中身と比較しながらレジスタを割り付けます。

## x86アセンブラ出力 emit

ここでは構造を変更することはないので、Progのポインタを受け取ってループして出力します。

