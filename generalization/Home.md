http://okmij.org/ftp/ML/generalization.html の翻訳

# OCamlの型検査の働き もしくは多相性とガベージコレクションが共通して持つ物

[](## How OCaml type checker works -- or what polymorphism and garbage collection have in common)

[](There is more to Hindley-Milner type inference than the Algorithm W.)
アルゴリズムWと言っただけではヒンドリー·ミルナーの型推論の説明として不十分です。
[](In 1988, Didier Rémy was looking to speed up the type inference in Caml and discovered an elegant method of type generalization. )
1988年ディディエ·レミー(Didier Rémy)はCamlの型推論を高速化するために型推論のエレガントな方法を発見しました。
[](Not only it is fast, avoiding the scan of the type environment.)
この方法は高速なだけではなく型環境のスキャンを回避します。
[](It smoothly extends to catching of locally-declared types about to escape, to type-checking of universals and existentials, and to implementing MLF.)
これは、ローカルに宣言された型のエスケープの捕捉や、普遍的で実存の型チェックそしてMLFの実装を滑らかに拡張します。 

[](Alas, both the algorithm and its implementation in the OCaml type checker are little known and little documented.)
悲しいかな、OCamlの型検査アルゴリズムとその実装の両方があまり知られておらず、ドキュメントもあまりありません。
[](This page is to explain and popularize Rémy's algorithm, and to decipher a part of the OCaml type checker.)
このページではレミのアルゴリズムの説明と普及を行い、OCamlの型チェックの一部を解読します。
[](The page also aims to preserve the history of Rémy's algorithm.)
このページは、レミーのアルゴリズムの歴史を保存することを目指しています。

[](The attraction of the algorithm is its insight into type generalization as dependency tracking -- the same sort of tracking used in automated memory management such as regions and generational garbage collection. )
このアルゴリズムの魅力は、依存関係の追跡などの型の一般化への洞察です - 同じ様な追跡手法は、リージョンおよび世代別ガベージコレクションなどの自動メモリー管理で使用されます。
[](Generalization can be viewed as finding dominators in the type-annotated abstract syntax tree with edges for shared types.)
一般化は、共有型のエッジを持つ型注釈付き抽象構文木に所有者を見つけることとみなすことができます。
[](Fluet and Morrisett's type system for regions and MetaOCaml environment classifiers use the generalization of a type variable as a criterion of region containment.)
FluetとMorrisettのリージョン型システムや、MetaOCaml環境分類器ではリージョン封じ込めの判定基準として型変数の一般化を使用しています。
[](Uncannily, Rémy's algorithm views the region containment as a test if a type variable is generalizable.)
不思議なことに、レミーのアルゴリズムは型変数が一般化されたリージョン包含テストとみなせます。

- [1. Introduction はじめに](1.intoro.md)
- [2. Generalization 一般化](2.一般化.md)
- [3. Unsound generalization as memory mismanagement メモリ管理ミスがある不完全な一般化](3.メモリ管理ミスがある不完全な一般化.md)
- [4. Efficient generalization with levels レベルによる効率的な一般化](4.レベルによる効率的な一般化.md)
- [5. Even more efficient level-based generalization さらに効率的なレベルベースの​​一般化](5.さらに効率的なレベルベースの​​一般化.md)
- [7. Type Regions 型リージョン](7.型リージョン.md)
- [8. Discovery of levels レベルの発見](8.レベルの発見.md)
 
[](Inside OCaml type checker)
OCaml内の型検査器

- [6. Generalization with levels in OCaml OCaml内レベル一般化](6.OCaml内レベル一般化.md)
- [7. Type Regions 型リージョン](7.型リージョン.md)
- [9. Creating fresh type variables 新鮮な型変数の生成](9.新鮮な型変数の生成.md)
- [a. The true complexity of generalization 真の一般化の複雑さ](a.真の一般化の複雑さ.md)