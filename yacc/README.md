# yaccを使おう

flex, bisonを使ってパーサを作って計算をしてみます。
[calc_ast](calc_ast)のみを見てわかるのであれば、[calc_ast](calc_ast)のみをコピると良いでしょう。

## [calc_check](calc_check)

文法チェックのみをします。

## [calc](calc)

パース時に計算を行います。

## [calc_ast](calc_ast)

パース結果の抽象構文木に保存し、表示します。

## [lambda](lambda)

クロージャのあるλ計算

## [cpp_arc_ast_eval](cpp_arc_ast_eval)

cppで使う例なのだけど、Objective-CのARCのような感じでオートリリースプールを使ってメモリ管理します。
ただの四則演算ですけど、とりあえず楽に作れます。

