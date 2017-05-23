# ATS ビューティファイア

## 仕組み

ネストを変更するためのプログラムにとって、全体の正しい文法を把握する必要はありません。
例えば、C言語のプリプロセッサは、C言語の文法をすべて解析しませんが、マクロ展開できます。
XHTMLのビューテファイアを作成するには、XHTMLはXMLのサブセットなのでXMLのビューテファイアを作れば十分事足ります。

一般的に、言語のフォーマッタを作成する場合には、対象言語のスーパーセットとなる言語を把握しそのスーパーセットのフォーマッタを作成できれば十分なのです。
スーパーセットである言語を生け垣言語と言います。

ML系の言語のプリティプリントが難しいのはその生け垣となる言語を把握しにくいところにあります。
ここでのATSビューテファイアの作成の試みは、そのML系統の生け垣となる言語を把握することで、フォーマッタを簡単に作成できるようにする事です。

まず、ATSの簡易的なパーサコンビネータを作成します。
通常、パーサを作成する際には字句情報や、コメント情報は削除します。
しかしながら、ビューテファイアを作成する上では、コメント情報や空白情報を残す必要があります。
したがって、ここで作成するパーサコンビネータは自動的にコメント情報や空白情報を残すような仕組みを組み込みました。
また、ネストをどこで入れるかの情報を文法に加えることで、ネストをうまく追加することが出来る仕組みも追加しました。

このプリティプリンタ生成用フレームワークを使えば、どのような言語のプリティプリンタもかなり楽に作成することが出来ます。

# super set of sub set of ats grammer

lexcal grammer

    keywords   ::= "begin" | "end" | "if" | "else" | "then" | "let" | "in" | "val" | "implement" | "local"
                |  "typedef" | "sortdef" | "datatype" | "extern" | "lam" | "try" | "with" | "fnx" | "fn"
                |  "fun" | "case" | "of" | "orelse" | "macrodef" | "macdef" | "staload" | "dynload" | "open"
                |  "struct" | "module" | "and" | " while" | "do" | "done"
    semi       ::= ";"
    exp        ::= exp4 (semi exp4)* semi?
    exps       ::= not keyword & exp semi?
    id         ::= ("$" | "\")? ( "_" | "a" ... "z" | "A" ... "Z" | "0" ... "9")+
                |  ":<cloref>"
                |  ("+" | "-" | "*" | "/" | "." | "<" | ">" | ":" | "@" | "=" | "^" | "|" | "~" | "?")+
                   & not("=>" | "=<" | "=" | "->" | "|")
                |  "!"
                |  ('"' ( "\" . | '"')* '"' | "'" ( "\" . | not "'" ) "'")

context grammer

    assign     ::= app "=" -exp
    sexp       ::=
                |  "@(" -exp? ")"
                |  "@[" -exp? "]"
                |  "'(" -exp? ")"
                |  ",(" -exp? ")"
                |  "'[" -exp? "]"
                |  "(" -exp? ")"
                |  "[" -exp? "]"
                |  "begin" -exp? "end"
                |  "{" -toplevel* "}"
                |  "@{" -(assign ("," assign)*) "}"
                |  "'{" -(assign ("," assign)*) "}"
                |  id
    app        ::= sexp+
    exp1       ::= "lam" -app ("=>" | "=<cloptr1>" | "=<cloref>") -exp2
                |  "let" -(toplevel ";;"?)+ "in" -exp? "end"
                |  "if" -exps "then" -exp4 ("else" exp)?
                |  "case"  -exps "of"   "|"? -app "=>" -exp ("|" app "=>" -exp)*
                |  "try"   -exps "with" "|"? -app "=>" -exp ("|" app "=>" -exp)*
                |  "while" -exps "do" -exps "done"
                |  app
    exp2       ::= exp1 (("=" | "orelse") exp2)?
    exp3       ::= exp2 ("," exp2)*
    exp4       ::= exp3 ("->" exp)?
    struct     ::= "struct" -prog "end" | -struct_exp
    struct_exp ::= ( id | "(" -opt(struct) ")")+
    datatype   ::= app "=" "|"? -app ("of" -app)? ("|" -app ("of" -app)?)*
    toplevel   ::= ("fn" | "fnx" | "fun" | "macdef" | "macrodef") -app ("=" -exp)?
                |  ("val" | "implement" | "typedef" | "sortdef" | "and") -app "=" -exp
                |  "extern" -(("fn" | "fnx" | "fun") app)
                |  ("#include" | "staload" | "dynload") -exp
                |  "#define" -sexp ("(" -exps ")") -sexp
                |  "local" -prog "in" -prog "end"
                |  "datatype" -datatype ("and" -datatype)*
                |  "exception" -id "of" -exp
                |  "open" -(id ("." id)*)
                |  exp semi?
                |  "module" -app "=" struct
    prog       ::= toplevel*

