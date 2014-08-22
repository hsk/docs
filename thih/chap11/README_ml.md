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

ここでは、1つの型inferについて説明します。

### type infer

	  type ('e, 't) infer = ti -> classEnv -> assump list -> 'e -> pred list * 't

この型は、型推論は2つの型 'e と 'tを使った関数を表しています。

- ti TIMonadで定義されているti 要するに型推論の文脈で、substとintの対
- classEnv クラス環境
- assump list 前提条件リスト
- 'e 型推論元の型

を受け取って

- pred list * 't pred listと型推論後の型

を返す関数が型推論の関数の方である事を示しています。
