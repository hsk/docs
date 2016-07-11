# coqのモジュール


ocamlのモジュールはa.mlのモジュール名はAになります。
対して、Coqのモジュール名は大文字小文字の区別があり、coqcに渡すファイル名が優先されるようです。

coq_makefileを使ってみると

```
coq_makefile abc/a.v b.v > Make
$ make -f Make
"coqdep" -c -R "." Top -I "." "b.v" > "b.v.d" || ( RV=$?; rm -f "b.v.d"; exit ${RV} )
"coqdep" -c -R "." Top -I "." "abc/a.v" > "abc/a.v.d" || ( RV=$?; rm -f "abc/a.v.d"; exit ${RV} )
"coqc"  -q  -R "." Top -I "."   abc/a
"coqc"  -q  -R "." Top -I "."   b
```

というようなことになるので、

```
$ coqc -R . Top abc/a b
```

とコンパイルするとうまくいくようです。
