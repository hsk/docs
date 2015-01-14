# minocaml9

字句解析を削る

## compiler-libs 使用例



OCaml 4.02.1 のcompiler-libsのパーサを書き換える事が出来るようにしてみました。
以下のページを参考にしました。

- https://bitbucket.org/camlspotter/compiler-libs-hack
- http://mzp.hatenablog.com/entry/2014/05/02/232753

## 作業内容

- vanillaを作ってみる。
- ソースコード見てそれなりに把握する。

- mzpさんの字句解析を変えている例をクローンしてみる。
- バージョン違いで動かないので修正する。

- 動いたので、次にファイル名の変更と、何も変わってない状態に戻す。

- 次にパーサを加えて、通常のパーサをカスタマイズ用パーサに置き換える。

これをあとは変更すれば好きに改造出来ます。
