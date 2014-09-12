## 11.5 program

ここでは1つの型programと1つの関数tiProgramを読みます。

### type program

プログラム全体を表す型です。

	  type program = bindGroup list

プログラムは変数束縛グループのリストに過ぎないわけです。

### show関数


#### 使用例


### tiProgram 関数

	  let tiProgram (ce:classEnv) (as_:assump list) (bgs : program):assump list =
	    runTI begin fun ti ->
	      let (ps, as') = tiSeq tiBindGroup ti ce as_ bgs in
	      let s = getSubst ti in
	      let rs = reduce ce (predsApply s ps) in
	      let s' = defaultSubst ce [] rs in
	      assumpsApply (s' @@ s) as'
	    end

プログラムの型推論は、クラス環境ce、前提条件as_、プログラムbgsを受け取って前提条件リストを返します。

runTIはTIMonadで定義された関数です。実行すると、型推論の文脈であるtiを渡されるので、処理して返します。

	(* module TIMonad 内のtiとrunTI *)
	  type ti = subst ref * int ref

	  let runTI (f : ti -> 'a):'a =
	    f (ref nullSubst, ref 0)

文脈tiはocaml版では、破壊的変更が可能な変数なので、ドンドン書き換えてしまうわけです。
そして、tiSeqで型推論します。

tiSeqで推論した結果、psとas'を受け取ります。
getSubst tiで文脈から代入を取り出し、代入 sをpsのなかから取り出したのがrsで
s'はdefaultSubstを使ってrsから取り出したsubstです。最後に、assumpsApplyでassump listを取得して返します。

要するに、tiProgramではprogramと環境を受け取って、TIMonad.runTIで環境を作ってtiSeqで型推論し、結果をassump listに変換して返します。

#### 使用例


todo:参考文献を書く
