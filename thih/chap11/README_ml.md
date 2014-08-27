# 11 型推論

この章でプログラムは完成します。

最終的には、TIMain.tiProgram関数が全体のエントリポイントになります。

## Inferモジュール

	(* 11 Type Inference *)
	module Infer = struct
	  open Pred
	  open Assump
	  open TIMonad
	  ...
	end

ここでは、1つの型inferについて読みます。

### type infer

	  type ('e, 't) infer = ti -> classEnv -> assump list -> 'e -> pred list * 't

この型は、型推論は2つの型 'e と 't を使った関数を表しています。

- ti TIMonad で定義されている ti 要するに型推論の文脈で、 subst と int の対
- classEnv クラス環境
- assump list 前提条件リスト
- 'e 型推論元の型

を受け取って

- pred list * 't pred list と型推論後の型

を返す関数が型推論の関数の方である事を示しています。

#### 使用例

これは、型しかないし、11.1のtiLitはtype inferではないか。

	  let tiLit (ti:ti) (lit:literal):pred list * type_

tiPatもtype inferではない。

	  let rec tiPat (ti:ti) (pat:pat):pred list * assump list * type_


tiExprは、'eがexprで'tがtype_のtype inferです。

	  let rec tiExpr (ti:ti)(ce:classEnv)(as_:assump list)(expr: expr): pred list * type_ =
	    begin match expr with

この型を定義すると大体の、型推論の関数のパターンが分かるわけですが、この型がないといけないわけでもないのです。
型クラスの定義とか、インターフェイスを定義する役割りなわけですね。

