# doctestを使ってみる

## インストール

```
cabal install doctest
```

## ソース

```
module A where
-- |
-- Id.
--
-- >>> id1 "a"
-- "a"
id1 a = a 
```
a.hs

## 実行

```
doctest a.hs
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

## 参照

[Haskellの単体テスト最前線](https://github.com/kazu-yamamoto/unit-test-example/blob/master/markdown/ja/tutorial.md)
