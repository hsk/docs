# 9 前提条件

### Assumpモジュール

	(* 9 Assumptions *)
	module Assump = struct
	  open Scheme
	  open Type
	  open Kind
  	  ...
	end

ここでは、1つの型assumpと5つの関数assumpApply,assumpTv,assumpsApply,assumpsTv,findを読みます。


### type assump

	  type assump = Assump of Id.id * scheme

### show 関数

	  let show (Assump(i, sc):assump) : assump =
	    Printf.sprintf "Assump(%s, %s)" id (Scheme.show sc)

#### 使用例

	  let _ =
	    let t = TVar(Tyvar("a", Star)) in
	    let assump = Assump("ABC", Forall([],Pred.Qual([],t))) in
	    Printf.printf "show %s\n" (show assump)

### show_list 関数

	  let show_list (assumps: assump list) : string =
	    Pre.show_list show ";" assumps

#### 使用例

	  let _ =
	    let t = TVar(Tyvar("a", Star)) in
	    let assump = Assump("ABC", Forall([],Pred.Qual([],t))) in
	    Printf.printf "show_list %s\n" (show_list [assump])

### assumpApply 関数

todo:説明を書く

	  let assumpApply (s:Subst.subst) (Assump(i, sc):assump) : assump =
	    Assump(i, schemeApply s sc)

#### 使用例

	  let _ =
	    let t = TVar(Tyvar("a", Star)) in
	    let assump = Assump("ABC", Forall([],Pred.Qual([],t))) in
	    let subst = [Tyvar("a", Star), tInt] in
	    let assump = assumpApply subst assump in
	    Printf.printf "assumpApply %s\n" (show assump)

### assumpTv 関数

todo:説明を書く

	  let assumpTv (Assump(_, sc):assump):Type.tyvar list =
	    schemeTv sc

#### 使用例

	  let _ =
	    let t = TVar(Tyvar("a", Star)) in
	    let assump = Assump("ABC", Forall([],Pred.Qual([],t))) in
	    let tvs = assumpTv assump in
	    Printf.printf "assumpTv %s\n" (Subst.show_tyvar_list tvs)

### assumpsApply 関数

todo:説明を書く

	  let assumpsApply (s:Subst.subst) (ass:assump list): assump list =
	    Subst.listApply assumpApply s ass

#### 使用例

	  let _ =
	    let t = TVar(Tyvar("a", Star)) in
	    let assump = Assump("ABC", Forall([],Pred.Qual([],t))) in
	    let subst = [Tyvar("a", Star), tInt] in
	    let _ = assumpsApply subst [assump] in
	    (*Printf.printf "show %s\n" (show assumps)*)
	    ()

### assumpsTv 関数

todo:説明を書く

	  let assumpsTv (ass:assump list): Type.tyvar list =
	    Subst.listTv assumpTv ass

#### 使用例

	  let _ =
	    let t = TVar(Tyvar("a", Star)) in
	    let assump = Assump("ABC", Forall([],Pred.Qual([],t))) in
	    let tvs = assumpsTv [assump] in
	    Printf.printf "assumpsTv %s\n" (Subst.show_tyvar_list tvs)

### find 関数

todo:説明を書く

	  let find (i:Id.id) (ass:assump list): scheme =
	    let Assump(_, sc) = List.find begin fun (Assump(i', _)) ->
	      i = i'
	    end ass in
	    sc

#### 使用例

	  let _ =
	    let t = TVar(Tyvar("a", Star)) in
	    let assump = Assump("ABC", Forall([],Pred.Qual([],t))) in
	    let sc = find"ABC"[assump]in
	    Printf.printf "find %s\n" (Scheme.show sc)
