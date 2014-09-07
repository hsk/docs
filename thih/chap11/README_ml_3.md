## 11.3 TIMainモジュール

	(* 11.3 Expressions
	 * 11.4 Alternatives
	 * 11.5 From Types to Type Schemes
	 * 11.6 Binding Groups *)
	module TIMain = struct
	  open Kind
	  open Type
	  open Pred
	  open Subst
	  open TIMonad
	  open Infer
	  open Lit
	  open Pat
	  open Scheme
	  open Assump
	  ...
	end

ここでは、1つの型ambiguityと、
２つの変数numClasses,stdClasses,
５つの関数ambiguities,withDefaults,defaultedPreds,defaultSubst,splitを読みます。


このモジュールは、Typing Haskell In Haskellではいくつかに別れています。
しかし、OCamlでは、相互参照がある場合に、前方参照はできず、andで繋いで書かなくてはなりません。
そのため、まとめて説明します。


### type ambiguity

	  type ambiguity = tyvar * pred list

ambiguityは曖昧性という意味なのだけど、非決定性計算のAMBと関連づけて覚えて置くと覚えやすいのかも。

### show_amb 関数

	  let show_amb ((tv,preds):ambiguity) =
	    Printf.sprintf "ambibuity(%s, %s)" (Subst.show_tyvar tv) (Pred.ps preds)

### show_ambs 関数

	  let show_ambs ambs =
	    Pre.show_list show_amb ";" ambs

### ambiguities 関数

withDefaultsで使用しています。

	  let ambiguities (vs:tyvar list) (ps:pred list) : ambiguity list =
	    let vs' = Pre.diff (predsTv ps) vs in
	    map begin fun v ->
	      (v, filter begin fun p ->
	        mem v (predTv p)
	      end ps)
	    end vs'

#### 使用例

	  let _ =
	    let tvs = [Tyvar("a", Star)] in
	    let preds = [IsIn("Num", tInt);IsIn("B", tInt)] in
	    let ambs = ambiguities tvs preds in
	    Printf.printf "ambs %s\n" (show_ambs ambs)

### numClasses 変数

Num関連のクラスのIdのリストです。

	  let numClasses : Id.id list = [
	    "Num"; "Integral"; "Floating"; "Fractional"; "Real"; "RealFloat";
	    "RealFrac"]

#### 使用例

	  let _ =
	    Printf.printf "numClasses = \n";
	    List.iter begin fun id ->
	      Printf.printf "  %s\n" id
	    end numClasses

### stdClasses 変数

標準的なクラスのIdのリストです。numClassesも含まれています。

	  let stdClasses : Id.id list = [
	    "Eq"; "Ord"; "Show"; "Read"; "Bounded"; "Enum"; "Ix"; "Functor"; "Monad";
	    "MonadPlus"] @ numClasses

#### 使用例

	  let _ =
	    Printf.printf "stdClasses = \n";
	    List.iter begin fun id ->
	      Printf.printf "  %s\n" id
	    end stdClasses

### candidates 関数

	  let candidates (ce:classEnv) ((v, qs) : ambiguity): type_ list =
	    let is = map (fun (IsIn(i, _)) -> i) qs in
	    let ts = map (fun (IsIn(_, t)) -> t) qs in
	    if for_all (fun t -> t = TVar v) ts &&
	      exists (fun i -> mem i numClasses) is &&
	      for_all (fun i -> mem i stdClasses) is then
	      let isCandidate t' =
	        for_all (entail ce []) (map (fun i -> IsIn(i, t')) is) in
	      filter isCandidate ce.defaults
	    else []

#### 使用例

	  let _ =
	    let tv = Tyvar("a", Star) in
	    let subst = [tv,tInt] in
	    let amb = (tv,subst) in
	    let ts = map (candidates ce) amb in
	    Printf.printf "ts = %s\n" (Type.show_list ts)

### withDefaults 関数

	  let withDefaults (f:ambiguity list -> type_ list -> 'a)
	    (ce:classEnv) (vs:tyvar list) (ps:pred list):'a =
	    let vps = ambiguities vs ps in
	    let tss = map (candidates ce) vps in
	    if exists Pre.isEmpty tss then failwith "cannot resolve ambiguity"
	    else f vps (map hd tss)

#### 使用例

	  

### defaultedPreds 関数

	  let defaultedPreds (ce:classEnv) (vs:tyvar list) (ps:pred list):pred list =
	    withDefaults (fun vps ts -> concat (map snd vps)) ce vs ps

#### 使用例

	  

### defaultSubst 関数

	  let defaultSubst (ce:classEnv) (vs:tyvar list) (ps:pred list): subst =
	    withDefaults (fun vps ts -> combine (map fst vps) ts) ce vs ps

#### 使用例

	  

### split 関数

	  let split (ce:classEnv) (fs:tyvar list) (gs:tyvar list)
	    (ps:pred list): pred list * pred list =
	    let ps' = reduce ce ps in
	    let (ds, rs) =
	      partition begin fun p ->
	        for_all begin fun t ->
	          mem t fs
	        end (predTv p)
	      end ps' in
	    let rs' = defaultedPreds ce (fs @ gs) rs in
	    (ds, Pre.diff rs rs')

#### 使用例

	  
