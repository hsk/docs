# 9 前提条件

### Assumpモジュール

	(* 9 Assumptions *)
	module Assump = struct

	  open Scheme
	  ...
	end

ここでは、1つの型assumpと5つの関数assumpApply,assumpTv,assumpsApply,assumpsTv,findについて説明します。

### type assump

todo:説明を書く

	  type assump = Assump of Id.id * scheme

### assumpApply 関数

todo:説明を書く

	  let assumpApply (s:Subst.subst) (Assump(i, sc):assump) : assump =
	    Assump(i, schemeApply s sc)

### assumpTv 関数

todo:説明を書く

	  let assumpTv (Assump(_, sc):assump):Type.tyvar list =
	    schemeTv sc

### assumpsApply 関数

todo:説明を書く

	  let assumpsApply (s:Subst.subst) (ass:assump list): assump list =
	    Subst.listApply assumpApply s ass

### assumpsTv 関数

todo:説明を書く

	  let assumpsTv (ass:assump list): Type.tyvar list =
	    Subst.listTv assumpTv ass

### find 関数

todo:説明を書く

	  let find (i:Id.id) (ass:assump list): scheme =
	    let Assump(_, sc) = List.find begin fun (Assump(i', _)) ->
	      i = i'
	    end ass in
	    sc


todo:使用例を書く