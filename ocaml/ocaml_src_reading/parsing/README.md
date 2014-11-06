# only parsing compiling

パーサのみをコンパイルしてみます。
まずは、ocamlでparsetree.mliを通してみましょう。

    ocamlc terminfo.mli terminfo.ml warnings.mli warnings.ml location.mli location.ml asttypes.mli misc.mli misc.ml longident.mli longident.ml parsetree.mli

依存関係を拾って行くと、utils以下のファイルが必要になりましたのでコピって持って来ました。後はparsingディレクトリ内のファイルで済みました。

通ったので、printast.mlを動かしましょう。

    ocamlc terminfo.mli terminfo.ml warnings.mli warnings.ml location.mli location.ml asttypes.mli misc.mli misc.ml longident.mli longident.ml parsetree.mli printast.mli printast.ml

特に困る事はなく動きました!

次は、pprintast.mlを動かしてみましょう。


    ocamlc terminfo.mli terminfo.ml warnings.mli warnings.ml location.mli location.ml asttypes.mli misc.mli misc.ml longident.mli longident.ml parsetree.mli pprintast.mli pprintast.ml

こちらも追加するファイルはなく動きますね。

では、パーサを通してみましょう。

    ocamlc terminfo.mli terminfo.ml warnings.mli warnings.ml location.mli location.ml asttypes.mli misc.mli misc.ml longident.mli longident.ml parsetree.mli ast_helper.mli ast_helper.ml config.mli config.ml clflags.mli clflags.ml syntaxerr.mli syntaxerr.ml parser.mli parser.ml

ast\_helper等が必要になりました。

lexerを追加しましょう。

   ocamlc terminfo.mli terminfo.ml warnings.mli warnings.ml location.mli location.ml asttypes.mli misc.mli misc.ml longident.mli longident.ml parsetree.mli ast_helper.mli ast_helper.ml config.mli config.ml clflags.mli clflags.ml syntaxerr.mli syntaxerr.ml parser.mli parser.ml lexer.mli lexer.ml

lexerには追加ファイル無しで通りました。

これだけですね。

```
     437    2605   15264 ast_helper.ml
     363    2065   16135 ast_helper.mli
     907    4335   34572 ast_mapper.ml
     193    1064    8721 ast_mapper.mli
      49     168    1515 asttypes.mli
     112     794    5664 clflags.ml
      96     491    3251 clflags.mli
     143     520    5133 config.ml
     129     701    5373 config.mli
      61     265    2277 lexer.mli
     677    2718   21027 lexer.mll
     387    1693   11725 location.ml
     119     579    4013 location.mli
      41     210    1657 longident.ml
      22      96    1008 longident.mli
     343    1689    9992 misc.ml
     163    1160    7349 misc.mli
      62     232    2387 parse.ml
      21     105    1245 parse.mli
     134     351    2243 parser.mli
    2193    9135   73574 parser.mly
     837    3511   24683 parsetree.mli
    1424    6796   54926 pprintast.ml
     141     744    7329 pprintast.mli
     891    4158   28670 printast.ml
      22     112    1184 printast.mli
      80     321    2930 syntaxerr.ml
      33     140    1422 syntaxerr.mli
      23     107    1173 terminfo.ml
      23     115    1212 terminfo.mli
     499    2852   19298 warnings.ml
      87     488    3932 warnings.mli
   10712   50320  380884 total
```

コメントもありで、1万700行です。
ast\_mapperは使わずに済むので1万行を把握すればよいわけです。




