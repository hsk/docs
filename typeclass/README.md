# Type classes

# タイプ・クラス

Philip Wadler

Ｐ・ワドラー

## The Implicit Calculus: A New Foundation for Generic Programming

## 陰の微積分法：一般的なプログラミングの新しい基盤

Bruno C. D. S. Oliveira, Tom Schrijvers, Wontae Choi, Wonchan Lee, Kwangkeun Yi, Philip Wadler. Draft paper, 2014.

ブルーノC. D. S.オリベイラ、トムSchrijvers、Wontaeチェ、Wonchanリー、Kwangkeun一、Ｐ・ワドラー。召集令状、2014。

Generic programming (GP) is an increasingly important trend in programming languages. Well-known GP mechanisms, such as type classes and the C++0x concepts proposal, usually combine two features: 1) a special type of interfaces; and 2) implicit instantiation of implementations of those interfaces.

一般的なプログラミング（GP）は、プログラミング言語のますます重要な傾向です。有名なGPメカニズム（例えばタイプ・クラスとC++0x概念提案）は、通常2つの特徴を結合します： 1) 特別な種類のインターフェース;そして、2）それらのインターフェースの実施の潜在的な例示。



Scala implicits are a GP language mechanism, inspired by type classes, that break with the tradition of coupling implicit instantiation with a special type of interface. Instead, implicits provide only implicit instantiation, which is generalized to work for any types. Scala implicits turn out to be quite powerful and useful to address many limitations that show up in other GP mechanisms.

階implicitsはGP言語メカニズムです。そして、タイプ・クラス（特別な種類のインターフェースで継手潜在的な例示の伝統を破ります）の影響を受けます。その代わりに、implicitsは潜在的な例示だけを提供します。そして、それはどんなタイプのためにでも仕事に一般化されます。階implicitsは、全く強力で他のGPメカニズムで現れる多くの限界について述べるために役に立つことがわかります。



This paper synthesizes the key ideas of implicits formally in a minimal and general core calculus called the implicit calculus (\lambda_?), and it shows how to build source languages supporting implicit instantiation on top of it. A novelty of the calculus is its support for partial resolution and higher-order rules (a feature that has been proposed before, but was never formalized or implemented). Ultimately, the implicit calculus provides a formal model of implicits, which can be used by language designers to study and inform implementations of similar mechanisms in their own languages.

本紙は陰の微積分法と呼ばれている最小で一般的な必須微積分学で正式にimplicitsについての鍵となる考えを総合します（\lambda_？）、そして、それはそれの上で潜在的な例示をサポートしている起点言語を構築する方法を示します。微積分学の目新しさは、部分的な決議と高次規則（プロポーズされたが、決して正式のものにされなかったか、インプリメントされる特徴）に対するその支持です。最後に、陰の微積分法はimplicitsの形式的モデルを提供します。そして、それは勉強して、彼ら自身の言語で実現例に類似したメカニズムを知らせるのに言語デザイナーによって用いられることができます。



Available in: pdf.

以下で利用できる：pdf.

## A second look at overloading

## オーバーローディングの2件目の観察

Martin Odersky, Philip Wadler, Martin Wehr. 7'th International Conference on Functional Programming and Computer Architecture, ACM Press, San Diego, California, June 1995.

マーティンOdersky、Ｐ・ワドラー、マーティン・ベーア。関数型プログラミングとコンピュータ建築、ACMプレス、サンディエゴ、カリフォルニア、6月の7'th国際会議1995.

We study a minimal extension of the Hindley/Milner system that supports overloading and polymorphic records. We show that the type system is sound with respect to a standard untyped compositional semantics. We also show that every typable term in this system has a principal type and give an algorithm to reconstruct that type.

我々は、オーバーローディングと多形記録をサポートするハインドリー/ミルナー・システムの最小の拡張を研究します。我々は、タイプ・システムが標準的な非入力された構成意味論に関する音であることを示します。我々も、このシステムのtypableな学期ごとには主要なタイプがあることを示して、そのタイプを再構築するために、アルゴリズムを伝えます。



Available in: dvi, ps, dvi.gz, ps.gz.

以下で利用できる：dvi, ps, dvi.gz, ps.gz.

## Type classes in Haskell

## ハスケルのタイプ・クラス

Cordelia Hall, Kevin Hammond, Simon Peyton Jones, and Philip Wadler. European Symposium On Programming, LNCS 788, Springer Verlag, pp. 241-256, April 1994.

コーデリア・ホール、ケビン・ハモンド、サイモン・ペイトン・ジョーンズとＰ・ワドラー。プログラミング、LNCS 788、はねる人Verlagの上のヨーロッパのシンポジウム、241-256、1994年4月に、pp.。

This paper defines a set of type inference rules for resolving overloading introduced by type classes. Programs including type classes are transformed into ones which may be typed by the Hindley-Milner inference rules. In contrast to an other work on type classes, the rules presented here relate directly to user programs. An innovative aspect of this work is the use of second-order lambda calculus to record type information in the program.

本紙は、タイプ・クラスによって導入されるオーバーローディングを分解することに対する一組のタイプ推論規則を定めます。タイプ・クラスを含むプログラムは、ハインドリー-ミルナー推論規則によって入力されるかもしれないものに変わります。タイプ・クラスの他の研究と対照的に、ここで提示される規則は、直接ユーザープログラムに関するものです。この仕事の革新的な面は、プログラムでタイプ情報を記録する第2の順序ラムダ微積分法の使用です。



Available in: dvi, ps, dvi.gz, ps.gz.

以下で利用できる：dvi, ps, dvi.gz, ps.gz.

## A static semantics for Haskell

## ハスケルのための静的意味論

Simon Peyton Jones and Philip Wadler. Draft paper, Glasgow, 1991.

サイモン・ペイトン・ジョーンズとＰ・ワドラー。召集令状、グラスゴー、1991。

Available in: dvi, ps, dvi.gz, ps.gz.

以下で利用できる：dvi, ps, dvi.gz, ps.gz.

## How to make ad-hoc polymorphism less ad hoc

## より特別に特別ポリエステル繊維モルフィズムを作らない方法

Philip Wadler and Stephen Blott. 16'th Symposium on Principles of Programming Languages, ACM Press, Austin, Texas, January 1989.

Ｐ・ワドラーとスティーブンBlott。 プログラミング言語、ACMプレス、テキサス州オースティン、1989年1月の原則に関する16'thシンポジウム。

This paper presents type classes, a new approach to ad-hoc polymorphism. Type classes permit overloading of arithmetic operators such as multiplication, and generalise the ``eqtype variables'' of Standard ML. Type classes extend the Hindley-Milner polymorphic type system, and provide a new approach to issues that arise in object-oriented programming, bounded type quantification, and abstract data types. This paper provides an informal introduction to type classes, and defines them formally by means of type inference rules.

本紙は、タイプ・クラス（特別ポリエステル繊維モルフィズムへの新しいアプローチ）を公演します。タイプ・クラスは掛け算のような算術演算子にオーバーロードすることを許可して、スタンダードMLの「eqtype変数」を一般化します。タイプ・クラスはハインドリー-ミルナー多形タイプ・システムを広げて、新しいアプローチをオブジェクト指向プログラミング、囲まれているタイプ定量化と抽象データ型で起こる問題に提供します。本紙は非公式の導入をタイプ・クラスに提供して、タイプ推論規則によって正式に彼らを定めます。



Available in: dvi, ps, dvi.gz, ps.gz.

以下で利用できる：dvi, ps, dvi.gz, ps.gz.

Philip Wadler,

Ｐ・ワドラー,  