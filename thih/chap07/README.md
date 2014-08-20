# 7 Type Classes, Predicates and Qualified Types 型クラス, 述語と限定型

One of the most unusual features of the Haskell type system, at least in comparison to those of other polymorphically typed languages like ML, is the support that it provides for type classes.

少なくともMLのような他の多相的に型付けされた言語のものと比較して、Haskellの型システムの最も珍しい特徴の一つは、型クラスのサポートです。

Described by Wadler and Blott [ Wadler & Blott, 1989] as a general mechanism that subsumes several ad-hoc forms of overloading, type classes have found many uses (and, sometimes, abuses!) in the ten years since they were introduced.

それらが導入されて以来のオーバーロードのいくつかのアドホックな形を包摂する一般的な機構としてWadlerとBlott[Wadler＆Blott、1989]で説明された、型クラスは（そして、時には、乱用され！）10年間での多くの用途が見出されています。

A significant portion of the code presented in this paper, particularly in this section, is needed to describe the handling of type classes in Haskell.

特に、このセクションで、本論文で示したコードの大部分は、Haskellで型クラスの処理を記述するために必要とされます。

(

Of course, type classes are not the only source of complexity. 

もちろん、型クラスが唯一の複雑さの元ではありません。

The treatment of mixed implicit and explicit typing, mutually recursive bindings, and pattern matching-which are often elided in more theoretical presentations-are also significant contributors, as is the extra level of detail and precision that is needed in executable code.

実行可能コードで必要とされる詳細と精度の余分なレベルがあるような混合暗黙的および明示的な型付け、相互再帰的な束縛、パターンマッチングは、多くの場合、また、重要な貢献者、であるプレゼンテーションより理論的に省略されています。

)

