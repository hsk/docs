# doctestを使ってみる

## インストール

```
cabal install doctest
```

## ソース

```
{- |
# A.hs

ここにモジュールの詳細な記述を書く．

-}

module A where

{- |
## id1

渡された値をそのまま返します。

	>>> id1 "a"
	"a"
-}

id1 a = a 

{- |

## id2 

こんな感じで動きます。

	>>> id2 "b"
	"b"
	>>> id2 "c"
	"c"

-}
id2 a = a
```
a.hs

## 実行

```
doctest a.hs
Examples: 3  Tried: 3  Errors: 0  Failures: 0
```

## ドキュメント作成

## Haddockのインストール

```
cabal install haddock
```

## 実行

```
mkdir doc
haddock -h a.hs -o doc
```

## Markdown生成

```
php hmddoc.php a.hs
```

で、`a.md`が生成されます。



## 参照

[HaskellのJavadoc, Haddockを使う](http://succzero.hatenablog.com/entry/2014/02/25/021357)

[Haskellの単体テスト最前線](https://github.com/kazu-yamamoto/unit-test-example/blob/master/markdown/ja/tutorial.md)
