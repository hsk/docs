# 1 Introduction はじめに

Haskell [2] benefits from one of the most sophisticated type systems of any widely used programming language.

Haskell [2] は最も洗練されたいずれかの広く使われているプログラミング言語の型システムのいずれかから恩恵を受けています。

Unfortunately, it also suffers because there is no formal specification of what the type system should be.

型システムがどうあるべきかの正式な仕様がないので残念ながら、それはまた欠点です。

As a result:

結果として：

- It is hard for Haskell implementors to be sure that their systems accept the same programs as other implementations.

	Haskellの実装者は、それらのシステムが、他の実装と同じプログラムを受け入れることを確認することは困難である。 

	The informal specification in the Haskell report [ Peyton Jones & Hughes, 1999]leaves too much room for confusion and misinterpretation.

	Haskellのレポートで非公式仕様[ペイトン·ジョーンズ＆ヒューズは、1999]は、混乱や誤解のためにあまりにも多くの余地がある。 

	This leads to genuine discrepancies between implementations, as subscribers to the Haskell mailing list will have seen.

	Haskellのメーリングリストに加入者が見てきたので、これは、実装の間、本物の矛盾につながる。 

- It is hard for Haskell programmers to understand the details of the type system and to appreciate why some programs are accepted when others are not.

	Haskellのプログラマは型システムの詳細を理解し、他の人がいないとき、いくつかのプログラムが受け入れられている理由を認めることは困難である。 

	Formal presentations of most aspects of the type system are available, but they often abstract on specific features that are Haskell-like, but not Haskell-exact, and do not describe the complete type system.

	型システムの多くの側面の正式な発表は利用可能ですが、Haskellの様な特定の機能についての彼らしばしば抽象的、しかしハスケル - 正確ではありませんし、完全な型システムを記載していない。 

	Moreover, these papers tend to use disparate and unfamiliar technical notation and concepts that may be difficult for some Haskell programmers to understand.

	さらに、これらの論文は、いくつかのHaskellのプログラマが理解しにくいかもしれ異種となじみのない技術的な記法や概念を使用する傾向がある。 

- It is hard for Haskell researchers to explore new type system extensions, or even to study usability issues that arise with the present type system such as the search for better type error diagnostics.

	Haskellの研究者はそのような優れたタイプのエラー診断のための検索した本型システムで発生するユーザビリティの問題を研究するためであっても、新しいタイプのシステム拡張を探索したりすることは困難である。 

	Work in these areas requires a clear understanding of the type system and, ideally, a platform on which to build and experiment with prototype implementations.

	これらの分野における研究は、理想的には、構築し、プロトタイプの実装を試して上のプラットフォームを、型システムを明確に理解する必要があります。 

	The existing Haskell implementations are not suitable for this (and were not intended to be): the nuts and bolts of a type system are easily obscured by the use of clever data structures and optimizations, or by the need to integrate smoothly with other parts of an implementation.

	既存のHaskellの実装は、このためには適していません（そして、あることを意図していなかった）。簡単に賢いデータ構造と最適化を使用することによって隠されているナットと型システムのボルト、またはの他の部分と滑らかに統合する必要性実装。


This paper presents a formal description of the Haskell type system using the notation of Haskell itself as a specification language.

本論文では、仕様記述言語としてはHaskell自体の表記を使用してHaskellの型システムの形式的な記述を提示します。 

Indeed, the source code for this paper is itself an executable Haskell program that is passed through a custom preprocessor and then through LATEX to obtain the typeset version.

実際、この論文のソースコードは、それ自体がタイプセットのバージョンを取得するためにLATEXを通じて、カスタムプリプロセッサを通過している実行可能なHaskellのプログラムです。 

The type checker is available in source form on the Internet at http://www.cse.ogi.edu/~mpj/thih/.

型チェッカーは http://www.cse.ogi.edu/~mpj/thih/ でインターネット上のソースの形で提供されています。

We hope that this will serve as a resource for the Haskell community, and that it will be a significant step in addressing the problems described previously.

私たちは、これがHaskellのコミュニティのためのリソースとして機能し、それは、前述した問題に対処する上で重要な一歩になることをことを願っています。

------

One audience whose needs may not be particularly well met by this paper are researchers in programming language type systems who do not have experience of Haskell.

そのニーズに特によくこの論文によって満たされない場合がありハスケルの経験を持っていないプログラミング言語タイプのシステムの研究者である一の観客。

(Of course, we encourage anyone in that position to learn more about Haskell!) Indeed, we do not follow the traditional route in such settings where the type system might first be presented in its purest form, and then related to a more concrete type inference algorithm by soundness and completeness theorems.

確かに、私たちは、型システムは、まず、その純粋な形で提示されることがありそのような設定で、伝統的なルートに従わない（もちろん、私たちは！ハスケルについての詳細を学ぶその位置に誰を奨励）、その後、より具体的なタイプに関連健全性と完全性定理によって、推論アルゴリズム。 

Here, we deal only with type inference.

ここでは、型推論でのみ扱う。

It does not even make sense to ask if our algorithm computes `principal' types: such a question requires a comparison between two different presentations of a type system, and we only have one.

それも、私たちのアルゴリズムは、 `校長のタイプを計算した場合に依頼する意味がありません：そのような質問は型システムの2つの異なったプレゼンテーションの間の比較を必要とし、私たちは1つしかない。

Nevertheless, we believe that our specification could be recast in a standard, type-theoretic manner and used to develop a presentation of Haskell typing in a more traditional style.

それにもかかわらず、私たちは私たちの仕様が標準、タイプ理論的な方法で再計算し、より伝統的なスタイルでHaskellのタイピングの提示を開発するために使用することができると信じています。 

--------

The code presented here can be executed with any Haskell system, but our primary goals have been clarity and simplicity, and the resulting code is not intended to be an efficient implementation of type inference.

ここで紹介するコードは、任意のHaskellシステムで実行することができますが、私たちの主な目標は明快さとシンプルされており、結果のコードは、型推論の効率的な実装であることを意図されていません。 

Indeed, in some places, our choice of representation may lead to significant overheads and duplicated computation.

実際、いくつかの場所で、表現の私たちの選択が重要なオーバーヘッド重複計算につながる可能性があります。

It would be interesting to try to derive a more efficient, but provably correct implementation from the specification given here.

それは、ここで与えられた仕様から、より効率的な、しかし証明可能正しい実装を導き出すことを試みるために興味深いものになるだろう。 

We have not attempted to do this because we expect that it would obscure the key ideas that we want to emphasize.

私たちは、それが私たちが強調したい重要なアイデアを不明瞭にすることを期待するので、私たちはこれを実行しようとしていません。 

It therefore remains as a topic for future work, and as a test to assess the applicability of program transformation and synthesis to modestly sized Haskell programs.

したがって、今後の作業のための課題として残り、テストとして緩やかにHaskellプログラムを大きさのためにプログラム変換と合成の適用性を評価する。 

--------

Another goal of this paper is to give as complete a description of the Haskell type system as possible, while also aiming for conciseness.

本論文のもう一つの目的は、また、簡潔さを目指しながら、可能な限りHaskellの型システムとしての完全な説明を与えることです。 

For this to be possible, we have assumed that certain transformations and checks will have been made prior to typechecking, and hence that we can work with a much simpler abstract syntax than the full source-level syntax of Haskell would suggest.


これを可能にするために、私たちは特定の変換やチェックは型チェックの前に行われていることを想定しているため、私たちはお勧めしますハスケルのフルソースレベルの構文よりもはるかに簡単な抽象構文で動作することが可能となる。

As we argue informally at various points in the paper, we do not believe that there would be any significant difficulty in extending our system to deal with the missing constructs.

私たちは、紙でのさまざまなポイントで非公式に議論するとして、私たちは不足している構造物に対処する私たちのシステムの拡張に有意な困難があるだろうとは思わない。 

All of the fundamental components, including the thorniest aspects of Haskell typing, are addressed in the framework that we present here.

Haskellのタイピングのthorniest側面を含む基本的な構成要素のすべてが、私たちはここで紹介するフレームワークで対処されています。 

Our specification does not attempt to deal with all of the issues that would occur in the implementation of a type checker in a full Haskell implementation.

当社の仕様では、完全なHaskellの実装では型チェックの実装で起こる問題のすべてに対処しようとしません。 

We do not tackle the problems of interfacing a typechecker with compiler front ends (to track source code locations in error diagnostics, for example) or back ends (to describe the implementation of overloading, for example), nor do we attempt to formalize any of the extensions that are implemented in current Haskell systems.

私たちは、コンパイラのフロントエンド（例えば、エラー診断でのソースコードの位置を追跡するために）、またはバックエンド（例えば、オーバーロードの実装を記述するため）との型検査器のインタフェースの問題に取り組む、また私たちは、任意のを形式化しようとしておりません現在のHaskellのシステムに実装されている拡張機能。

This is one of things that makes our specification relatively concise (429 lines of Haskell code).

これは、比較的簡潔な当社指定（Haskellコードの429行）を行うものの一つです。 

By comparison, the core parts of the Hugs typechecker take some 90+ pages of C code.

比較すると、抱擁のコア部分は、Cコードのいくつかの90 +のページを取る型検査器。 

--------

Some examples are included in the paper to illustrate the datatypes and representations that are used.

いくつかの例が使用されているデータ型と表現を説明するため、紙に含まれています。 

However, for reasons of space, the definitions of some constants that represent entities in the standard prelude, as well as the machinery that we use in testing to display the results of type inference, are included only in the electronic distribution, and not in the typeset version of the paper.

しかし、スペース上の理由から、標準プレリュード内のエンティティだけでなく、私たちは型推論の結果を表示するために、テストで使用する機械を表すいくつかの定数の定義は、唯一の電子分布のではなく、中に含まれている論文のタイプセットバージョン。 

Apart from those details, this paper gives the full source code.

別にこれらの詳細から、本稿では、完全なソースコードを提供します。 

--------

We expect the program described here to evolve in at least three different ways.

私たちは、少なくとも3つの異なる方法を中に進化し、ここで記述されたプログラムを期待しています。

- Formal specifications are not immune to error, and so it is possible that changes will be required to correct bugs in the code presented here.

	正式な仕様は、誤りの免疫ではありませんので、それは変更がここに記載するコードのバグを修正するために必要とされる可能性があります。 

	On the other hand, by writing our specification as a program that can be typechecked and executed with existing Haskell implementations, we have a powerful facility for detecting simple bugs automatically and for testing to expose deeper problems.

	一方、型検査、既存のHaskell実装で実行可能なプログラムとしての仕様を書くことによって、私たちは、自動的にテストが深い問題を露出するための単純なバグを検出するための強力な機能を持っています。 

- As it stands, this paper just provides one more interpretation of the Haskell type system.

	現状では、本論文では、単にHaskellの型システムのもう一つの解釈は、用意されています。 

	We believe that it is consistent with the official specification, but because the latter is given only informally, we cannot prove the correctness of our program in a rigorous manner.

	私たちは、それが正式な仕様と一致しているが、後者のみが非公式に与えられているので、私たちは厳格な方法で私たちのプログラムの正しさを証明することはできないと信じています。 

	Instead, we hope that our code, perhaps with some modifications, will eventually serve as a precise definition of the Haskell type system, capturing a consensus within the Haskell community.

	代わりに、私たちは私たちのコードは、おそらくいくつか変更を加えて、最終的にはHaskellのコミュニティの中でコンセンサスをキャプチャ、Haskellの型システムの正確な定義として役立つことを願っています。 

	There is some evidence that this goal is already within reach: no discrepancies or technical changes have been discovered or reported in more than a year since the first version of this program was released.

	齟齬や技術的な変更は、発見されなかったり、このプログラムの最初のバージョンがリリースされてから一年以上が報告されている：この目標は手の届くところに既にいくつかの証拠がある。 

- Many extensions of the Haskell type system have been proposed, and several of these have already been implemented in one or more of the available Haskell systems.

	Haskellの型システムの多くの拡張が提案されており、これらのいくつかは、すでに1つまたは複数の使用可能なHaskellのシステムの複数において実装されている。 

	Some of the better known examples of this include multiple-parameter type classes, existential types, rank-2 polymorphism, and extensible records.

	これのよりよい知られている例の一部は、多引数の型クラス、実存の種類、ランク2多型、および拡張可能なレコードが含まれています。 

	We would like to obtain formal descriptions for as many of these proposals as possible by extending the core specification presented here.

	私たちは、ここで紹介するコア仕様を拡張することで、可能な限りこれらの提案の多くのための正式な記述を得るためにしたいと思います。

It will come as no surprise to learn that some knowledge of Haskell will be required to read this paper.

それは、Haskellのいくつかの知識がこの論文を読むために必要になることを学ぶために驚きとして来る。 

That said, we have tried to keep the definitions and code as clear and simple as possible, and although we have made some use of Haskell overloading and do-notation, we have generally avoided using the more esoteric features of Haskell.

つまり、私たちは可能な限り明確かつシンプルな定義やコードを維持しようとしたと述べた、と私たちはとdo記法の過負荷をHaskellのいくつか利用してきたが、私たちは一般的にはHaskellのより難解な機能を使用して避けてきた。 

In addition, some experience with the basics of Hindley-Milner style type inference [ Hindley, 1969, Milner, 1978, Damas & Milner, 1982]will be needed to understand the algorithms presented here.

また、ヒンドリー-ミルナースタイルの型推論[ブレイクフリー1969、ミルナー、1978、ダマ＆ミルナー、1982]の基礎といくつかの経験がここで紹介するアルゴリズムを理解するために必要とされるであろう。 

Although we have aimed to keep our presentation as simple as possible, some aspects of the problems that we are trying to address have inherent complexity or technical depth that cannot be side-stepped.

私たちは可能な限りシンプルたちのプレゼンテーションを保つことを目的としているが、私たちが対処しようとしている問題のいくつかの側面は、サイドステップ実行することはできません固有の複雑さや技術的な深さを有する。 

In short, this paper will probably not be useful as a tutorial introduction to Hindley-Milner style type inference!

要するに、本論文は、おそらくヒンドリー-ミルナースタイルの型推論にチュートリアルの導入として有用ではありません！
