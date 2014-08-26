# 7 型クラス, 述語と限定型

## Predモジュール

	(* 7 Type Classes, Predicates and Qualified Types *)
	module Pred = struct
	  open Kind
	  open Type
	  open Subst
	  ...
	end

この章では6つの型と、30個の関数に付いて説明します。
分量が多いので、4つの説に分けてPredモジュールを説明します。

- 7.1では2つの型pred,qualと9つの関数をpredApply,predsApply,qualTypeApply,predTv,predsTv,qualTypeTv,lift,mguPred,matchPredを説明します。
- 7.2では4つの型inst, class_, classEnv, envTransformerと9つの関数super,insts,defined,modify,initialEnv,(<:>),addClass,overlap,addInst、4つの環境トランスフォーマーaddCoreClasses,addNumClasses,addPreludeClasses,exampleInstsを説明します。
- 7.3では3つの関数bySuper,byInst,entailについて説明します。
- 7.4では5つの関数inHnf,inHnfs,toHnf,simplify,reduceを説明します。

この章を終えれば、8,9,10は大した分量がないので楽になります。
先に8,9,10章を見ておくのも手でしょう。

また、7.2の環境トランスフォーマーの箇所だけを見るのは他とあまり絡まないので面白く読めると思います。

todo: なんか、数だけ書いてあってよくわからないのでなんとかする。
