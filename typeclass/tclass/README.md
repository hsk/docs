# 型クラスの実装例

- [mono](mono) 単相型推論
- [mono\_tclass](mono_tclass) 単相型推論+型クラス
- [poly](poly) 多相型推論
- [poly\_mono\_tclass](poly_mono_tclass) 多相型推論+単相型クラス
- [poly\_tclass](poly_tclass) 多相型推論+型クラス

型クラスとは、Haskellで導入されている多相的な処理を行う仕組みです。例えば、==演算子や+演算子を既存の型に対して複数の型で複数の処理をしたい場合、オブジェクト指向の場合は、値とメソッドをまとめたデータとして扱う事で解決します。一方、型クラスはデータとメソッドを別けて引き渡して解決します。型クラスを使うとメソッドと型は独立しているので静的に後から変更出来ます。ここでは、型クラスの実装方法についてソースコード付きで細かく問題を分けて詳細に渡って説明します。

## 型クラスをOCamlで実装する簡単な例

では、OCamlで型クラスを実装して見ましょう。

型クラスはHaskellでは以下のように定義出来ます:

	class Num where
	  add :: a -> a -> a
	instance Num Integer where
	  add = \a b -> a + b
	instance Num Double where
	  add = \a b -> a + b
	main = do
	  putStrLn $ show $ add 1 (2::Integer)
	  putStrLn $ show $ add 1.1 2.2
		
このプログラムは以下のようなOCamlのプログラムに変換出来ます:
	
	type 'a num = { add : 'a -> 'a -> 'a } in
	let add : 'a num -> 'a -> 'a -> 'a = fun num a b -> num.add a b in
	let num_int = { add = fun a b -> a + b } in
	let num_float = { add = fun a b -> a +. b } in
	print_int(add num_int 1 2);
	print_float(add num_float 1 2)
	

classの定義は、レコードの型と関数に書き換える事が出来ます。
instance定義は、レコードの値をバインドするlet式に変換出来ます。
オーバーロードされた値はレコードを受け取る式として変換出来ます。
オーバーロードされた値の関数定義はなくても変換出来ますが、関数にしたほうが処理が理解しやすくなります。
レコードは使わずに多相的な多値を使っても実装可能です。

