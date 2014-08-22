## 11.2 Pat モジュール

パターンを表すのがPatモジュールです。

	(* 11.2 Patterns *)
	module Pat = struct
	  open Kind
	  open Type
	  open Pred
	  open Scheme
	  open Assump
	  open TIMonad
	  open Infer
	  open Lit
	  ...
	end

ここでは、1つの型patと2つの関数tiPat,tiPatsについて説明します。

### type pat

	  type pat =
	    | PVar of Id.id
	    | PWildcard
	    | PAs of Id.id * pat
	    | PLit of literal
	    | PNpk of Id.id * big_int
	    | PCon of assump * pat list

色々なパターンが定義されています。

### tiPat 関数

	  let rec tiPat (ti:ti) (pat:pat):pred list * assump list * type_ =
	    begin match pat with
	      | PVar i ->
	        let t = newTVar ti Star in
	        ([], [Assump(i, toScheme t)], t)
	      | PWildcard -> ([], [], newTVar ti Star)
	      | PAs(i, pat) ->
	        let (ps, as_, t) = tiPat ti pat in
	        (ps, Assump(i, toScheme t) :: as_, t)
	      | PLit l ->
	        let (ps, t) = tiLit ti l in
	        (ps, [], t)
	      | PNpk(i, k) ->
	        let t = newTVar ti Star in
	        ([IsIn("Integral", t)], [Assump(i, toScheme t)], t)
	      | PCon(Assump(i, sc), pats) ->
	        let (ps, as_, ts) = tiPats ti pats in
	        let t' = newTVar ti Star in
	        let Qual(qs, t) = freshInst ti sc in
	        unify ti t (fold_right fn ts t');
	        (ps @ qs, as_, t')
	    end

パターンの型推論関数です。

### tiPats 関数

	  and tiPats (ti:ti) (pats:pat list):pred list * assump list * type_ list =
	    let (pss, ass, ts) = Pre.split3 (map (tiPat ti) pats) in
	    (concat pss, concat ass, ts)

パターンリストの型推論関数です。

todo:使用例を書く
