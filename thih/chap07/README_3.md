## 7.3 Entailment 含意

In this section, we describe how class environments can be used to answer questions about which types are instances of particular classes.

このセクションでは種類のある特定のクラスのインスタンスについての質問に答えるクラスの環境の使用方法を説明します。

More generally, we consider the treatment of entailment: given a predicate p and a list of predicates ps, our goal is to determine whether p will hold whenever all of the predicates in ps are satisfied.

一般的に、我々 は治療法を検討含意: 述語 p と述語 ps のリストを与え、私たちの目標はすべての ps の述語が満たされるたびに p を保持するかどうかを決定します。

In the special case where p = IsIn i t and ps = [], this amounts to determining whether t is an instance of the class i.

特殊なケースでどこで p = IsIn は t と ps = t がクラスのインスタンスかどうかを決定するのには、この金額私は。

In the theory of qualified types [ Jones, 1992], assertions like this are captured using judgements of the form ps ||- p; we use a different notation here-the entail function that is defined at the end of this section-to make the dependence on a class environment explicit.

修飾型 [ジョーンズ、1992年] の理論的には、このようなアサーションがキャプチャされますフォーム ps の判断を使用して繁体 | |-p;我々 はこのようにセクションに明示的なクラス環境依存の終わりに定義されている別の表記法ここに伴う関数を使用します。

As a first step, we can ask how information about superclasses and instances can be used independently to help reason about entailments.

最初のステップとして我々 どのようにスーパークラスとインスタンスについてことができる単独で使える含意 (entailment) に関する理由のために求めることができます。

For example, if a type is an instance of a class i, then it must also be an instance of any superclasses of i.

たとえば、型がクラスのインスタンスの場合、私はそれも私のスーパークラスのインスタンスをする必要があります。

Hence, using only superclass information, we can be sure that, if a given predicate p holds, then so too must all of the predicates in the list bySuper p:

したがって、スーパークラス情報のみを使用して、私たちすることができることを確認する、特定の述語 p 保持している場合、それからそれであまりにもすべてのリスト bySuper p: 述語のする必要があります

	  bySuper :: ClassEnv -> Pred -> [Pred]
	  bySuper ce p@(IsIn i t)
	   = p : concat [ bySuper ce (IsIn i' t) | i' <- super ce i ]

The list bySuper ce p may contain duplicates, but it will always be finite because of the restriction that the superclass hierarchy is acyclic.

リスト bySuper ce p は、重複するファイルを含めることができますがそれは常に有限スーパークラス階層が非環式制限があるため。

Next we consider how information about instances can be used.

次のインスタンスについての情報の使用方法を考えます。

Of course, for a given predicate p = IsIn i t, we can find all the directly relevant instances in a class environment ce by looking in insts ce i.

もちろん、指定した述語 p = IsIn は t、我々 クラス環境に直接関連するすべてのインスタンスを検索できます insts で見ることによって ce ce 私は。

As we have seen, individual instance declarations are mapped into clauses of the form ps :=> h.

ように、個々 のインスタンスの宣言は:=> h 形 ps の句にマップされます。

The head predicate h describes the general form of instances that can be constructed from this declaration, and we can use matchPred to determine whether this instance is applicable to the given predicate p.

頭部の述語 h この宣言から構築できるインスタンスの一般的な形式を説明し、このインスタンスが指定された述語 p に適用できるかどうかを確認するのに matchPred を使用することができます。

If it is applicable, then matching will return a substitution u, and the remaining subgoals are the elements of map (apply u) ps.

該当する場合は置換 u を戻ります一致する残りの下位はマップの要素 (u は適用) ps。

The following function uses these ideas to determine the list of subgoals for a given predicate:

次の関数指定された述語の下位のリストを決定するこれらのアイデアを使用します。

	  byInst                   :: ClassEnv -> Pred -> Maybe [Pred]
	  byInst ce p@(IsIn i t)    = msum [ tryInst it | it <- insts ce i ]
	   where tryInst (ps :=> h) = do u <- matchPred h p
	                                 Just (map (apply u) ps)

The msum function used here comes from the standard Monad library, and returns the first defined element in a list of Maybe values; if there are no defined elements in the list, then it returns Nothing.

ここで使用される msum 関数標準モナド ライブラリからが来るし、多分値; のリスト定義の最初の要素を返します。一覧で定義されている要素がない場合、Nothing が返されます。

Because Haskell prevents overlapping instances, there is at most one applicable instance for any given p, and we can be sure that the first defined element will actually be the only defined element in this list.

Haskell は重複するインスタンスを防ぎます、任意の与えられた p, 1 つの該当するインスタンスが多くて、私たちすることができることを確認します定義の最初の要素は、実際にありますこのリストの唯一の定義された要素。

The bySuper and byInst functions can be used in combination to define a general entailment operator, entail.

BySuper と byInst 関数は、一般的な含意演算子定義を伴うの組み合わせで使用できます。

Given a particular class environment ce, the intention here is that entail ce ps p will be True if, and only if, the predicate p will hold whenever all of the predicates in ps are satisfied:

特定のクラスの環境 ce では、ここでの意図を伴う ce ps および ps の述部のすべてが満たされるたびに、述語 p を保持する場合にのみ p は True になります。

	  entail        :: ClassEnv -> [Pred] -> Pred -> Bool
	  entail ce ps p = any (p `elem`) (map (bySuper ce) ps) ||
	                   case byInst ce p of
	                     Nothing -> False
	                     Just qs -> all (entail ce ps) qs

The first step here is to determine whether p can be deduced from ps using only superclasses.

スーパークラス化のみを使用して ps から p を推定できるかどうかを確認するのにはここでまずします。

If that fails, we look for a matching instance and generate a list of predicates qs as a new goal, each of which must, in turn, follow from ps.

失敗した場合、一致するインスタンスとそれぞれの ps から従う必要があります、順番に、新たな目標として述語 qs のリストを生成です。

Conditions specified in the Haskell report-namely that the class hierarchy is acyclic and that the types in any instance declaration are strictly smaller than those in the head-translate into conditions on the values for the ClassEnv that can be passed in as ce, and these are enough to guarantee that tests for entailment will terminate.

クラスの階層構造が非環式、任意のインスタンスの宣言に型が厳密にこれらのより小さい、Haskell レポート-すなわちで指定された条件、ce、としてで渡すことができます ClassEnv の変換値を条件に頭と含意のテストが終了することを保証するのに十分です。

Completeness of the algorithm is also important: will entail ce ps p always return True whenever there is a way to prove p from ps? In fact our algorithm does not cover all possible cases: it does not test to see if p is a superclass of some other predicate q for which entail ce ps q is True.

アルゴリズムの完全性も重要です: ps から p を証明する方法があるときに True ce ps p 常に戻り値を伴うだろうか？我々 のアルゴリズム実際にすべての可能なケースをカバーしていない： p はいくつか他の述語 q を伴うのための ce ps q は True スーパークラスにはテストしません。

Extending the algorithm to test for this would be very difficult because there is no obvious way to choose a particular q, and, in general, there will be infinitely many potential candidates to consider.

これをテストするアルゴリズムの拡張は特定の q を選択する明確な方法がないと、一般がある無限に多くの潜在的な候補として検討するため非常に難しいでしょう。

Fortunately, a technical condition in the Haskell report [ Peyton Jones & Hughes, 1999,Condition 1 on Page 47] reassures us that this is not necessary: if p can be obtained as an immediate superclass of some predicate q that was built using an instance declaration in an entailment entail ce ps q, then ps must already be strong enough to deduce p.

幸いなことに、Haskell report [ペイトン ジョーンズ & ヒューズ、1999 年、ページ 47 [条件 1] で技術的条件私たちを安心させるこれは必要ありません： p を使用して建てられたいくつか述語の q の直近のスーパークラスとして取得できる場合、含意のインスタンスの宣言を伴う ce ps q その後 ps はすでに p を推測するのに十分な強力なする必要があります。

Thus, although we have not formally proved these properties, we believe that our algorithm is sound, complete, and guaranteed to terminate.

したがって、我々 正式にこれらのプロパティを証明していない、我々 我々 のアルゴリズムは音、完了し、終了する保証と考えています。

