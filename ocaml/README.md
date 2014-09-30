
# odoctest

odoctestはodocのコメント内の>>>がある箇所のプログラムを実行し、
次の行の結果と比較します。

# mdファイル生成

-odocオプションを付けるとocamlのファイルの依存ファイルを解析し、
依存ファイルの全てのファイルについてmarkdownファイルを生成します。
mlファイル内の`(*|`と`*)`の間のコメントをmdファイルに出力します。

例)

```
$ odoctest a.ml -odoc
```

## 自動テスト結果書き換え

--rewrite=filesオプションを使うとテスト結果を自動で書き換えることが出来ます。
書き換えたファイルは.bakにバックアップが取られます。
書き換えるファイルはfilesの箇所にカンマ区切りの.mlの拡張子は無しで指定します。

例) a.mlを書き換える

```
$ odoctest a.ml --rewrite=a
```

例) b.mlを書き換える

```
$ odoctest a.ml --rewrite=b
```

例) a.mlとb.mlを書き換える

```
$ odoctest a.ml --rewrite=a,b
```

## rewrite all

--rewrite=allオプションを使うと全てのテストに関連するmlファイルのテスト結果を自動で書き換えることが出来ます。
書き換えたファイルは.bakにバックアップが取られます。

例) a.mlのテストに関連する全てのmlファイルを書き換える。

```
$ odoctest a.ml --rewrite=all
```