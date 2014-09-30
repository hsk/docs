(*

 This file is a translation and changing of the code pertaining to the article
 `Typing Haskell in Haskell' by Mark P. Jones [1], from Haskell to OCaml.
 Original OCaml file made by Cyril Soldani [2]. 

 1: http://web.cecs.pdx.edu/~mpj/thih/
 2: http://devmusings.legiasoft.com/_media/blog/2010/08/04/typinghaskellinml.ml
 
 Copyright (C) 1999 - 2000  Mark P. Jones
 Copyright (C) 2010 Cyril Soldani
 Copyrigtt (C) 2014 Hiroshi Sakurai

 Here follows the license (license of the original Haskell code from the
 article).

 `Typing Haskell in Haskell' is Copyright (c) Mark P Jones
 and the Oregon Graduate Institute of Science and Technology,
 1999-2000, All rights reserved, and is distributed as
 free software under the following license.

 Redistribution and use in source and binary forms, with or
 without modification, are permitted provided that the following
 conditions are met:

 - Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.

 - Redistributions in binary form must reproduce the above
 copyright notice, this list of conditions and the following
 disclaimer in the documentation and/or other materials provided
 with the distribution.

 - Neither name of the copyright holders nor the names of its
 contributors may be used to endorse or promote products derived
 from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND THE
 CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR THE
 CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

module Pre = struct
  (* 和集合 *)
  let union (xs: 'a list) (ys: 'a list):'a list =
    List.filter begin fun x ->
      not (List.mem x ys)
    end xs @ ys

  (* 積集合 *)
  let intersect (xs: 'a list) (ys: 'a list): 'a list =
    List.filter begin fun x ->
      List.mem x ys
    end xs

  (* リストをセットにする。要素が１つずつにまとめる *)
  let nub (xs : 'a list): 'a list =
    
    let ls =  List.fold_left begin fun ys x ->
        if List.mem x ys
        then ys
        else x :: ys
      end [] xs
    in
    List.rev ls

  let show_list show sep xs =
    begin
      let rec loop xs =
        begin match xs with
          | [] -> ""
          | [x] -> show x
          | x::xs -> show x ^ sep ^ loop xs
        end
    in
      Printf.sprintf "[%s]" (loop xs)
    end
  let show_int_list xs =
    show_list string_of_int "; " xs

  (* 空チェック *)
  let isEmpty(xs: 'a list):bool =
    begin match xs with
      | [] -> true
      | _ -> false
    end
  (* たぶんこれは、reduceじゃないのかな *)
  let fold_left1 (f:'a -> 'a -> 'a) (xs:'a list): 'a = 
    match xs with
    | [] -> invalid_arg "empty list"
    | [x] -> x
    | x :: xs -> List.fold_left f x xs

  (* リスト内の最初の1個目のxを削除する *)
  let rec deleteFirst (x:'a) (ys:'a list): 'a list = 
    begin match ys with
      | [] -> []
      | y :: ys ->
        if x = y then ys
        else y :: deleteFirst x ys
    end
  (* 最初のリストから2番目のリストの要素を消す *)
  let rec diff (xs:'a list) (ys:'a list): 'a list =
    begin match ys with
      | [] -> xs
      | y :: ys -> diff (deleteFirst y xs) ys
    end

  (*3つの多値を持っているリストを３つのリストに分割する *)
  let split3 (xs:('a * 'b * 'c)list):('a list * 'b list * 'c list) =
    let rec loop ((ws:'a list), (xs:'b list), (ys:'c list))
      (zs:('a * 'b * 'c)list):('a list * 'b list * 'c list) =
      begin match zs with
        | [] -> (List.rev ws, List.rev xs, List.rev ys)
        | (w, x, y) :: zs -> loop (w :: ws, x :: xs, y :: ys) zs
      end
    in
    loop ([], [], []) xs
end

(*|

    >>> open Thih;;
    

## union

    >>> (Pre.union [1;2] [2; 3]);;
    - : int list = [1; 2; 3]

## intersect

    >>> (Pre.intersect [1;2] [2; 3]) ;;
    - : int list = [2]

## union and intersect

    >>> let a = [5;4;3;2;1];;
    val a : int list = [5; 4; 3; 2; 1]

    >>> let b = [4;5;6;7];;
    val b : int list = [4; 5; 6; 7]

    >>> let ab = Pre.union a b;;
    val ab : int list = [3; 2; 1; 4; 5; 6; 7]
    
    >>> ab;;
    - : int list = [3; 2; 1; 4; 5; 6; 7]

    >>> let ab = Pre.intersect a b;;
    val ab : int list = [5; 4]

## nub

    >>> let a = [1;1;2;2;3;4;5;1];;
    val a : int list = [1; 1; 2; 2; 3; 4; 5; 1]

    >>> let a2 = Pre.nub a;;
    val a2 : int list = [1; 2; 3; 4; 5]

## is empty 1

    >>> let a = [];;
    val a : 'a list = []

    >>> Pre.isEmpty a;;
    - : bool = true

## is empty 2

    >>> let a = [1];;
    val a : int list = [1]

    >>> Pre.isEmpty a;;
    - : bool = false

## fold_left1

    >>> let a = [1;2;3;4;5;6;7;8;9;10];;
    val a : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

    >>> let a = Pre.fold_left1 begin fun a b -> a + b end a;;
    val a : int = 55

## deleteFirst

    >>> let a = [1;2;3;4;3;4;5] ;;
    val a : int list = [1; 2; 3; 4; 3; 4; 5]

    >>> let a = Pre.deleteFirst 3 a ;;
    val a : int list = [1; 2; 4; 3; 4; 5]

## diff

    >>> let a = [1;2;3;4];;
    val a : int list = [1; 2; 3; 4]

    >>> let b = [3;4;5;6];;
    val b : int list = [3; 4; 5; 6]

    >>> let a = Pre.diff a b;;
    val a : int list = [1; 2]

## diff 2

    >>> let a = [1;2;3;4;3;4];;
    val a : int list = [1; 2; 3; 4; 3; 4]

    >>> let b = [3;4;5;6;4];;
    val b : int list = [3; 4; 5; 6; 4]

    >>> let a = Pre.diff a b;;
    val a : int list = [1; 2; 3]

## split3

    >>> let a1 = [1,10,100;2,20,200;3,30,300];;
    val a1 : (int * int * int) list = [(1, 10, 100); (2, 20, 200); (3, 30, 300)]

    >>> let (a,_,_) = Pre.split3 a1;;
    val a : int list = [1; 2; 3]

    >>> let (_,b,_) = Pre.split3 a1;;
    val b : int list = [10; 20; 30]

    >>> let (_,_,c) = Pre.split3 a1;;
    val c : int list = [100; 200; 300]

    >>> a ;;
    - : int list = [1; 2; 3]

    >>> b;;
    - : int list = [10; 20; 30]

    >>> c;;
    - : int list = [100; 200; 300]

*)

module Id = struct
  type id = string

  (* 数値に対するidを取得する *)
  let enumId (n:int) : id =
    "v" ^ string_of_int n
end

(*|

## enumId 1

    >>> Id.enumId 1;;
    - : Thih.Id.id = "v1"

## enumId 33
    >>> Id.enumId 33;;
    - : Thih.Id.id = "v33"

*)

(* 3 Kinds *)
module Kind = struct
  type kind =
    | Star
    | Kfun of kind * kind

  let rec show (k:kind):string =
    begin match k with
      | Star -> "*"
      | Kfun(Kfun _ as k1,k2) -> Printf.sprintf "(%s) -> %s" (show k1) (show k2) 
      | Kfun(k1,k2) -> Printf.sprintf "%s -> %s" (show k1) (show k2) 
    end

  let rec show_list (ks:kind list):string =
    Pre.show_list show ";" ks
end

(*|

    >>> open Thih.Kind;;
    

    >>> Star ;;
    - : Thih.Kind.kind = Star

    >>> Kfun(Star, Star) ;;
    - : Thih.Kind.kind = Kfun (Star, Star)

    >>> Kfun(Star, Kfun(Star, Star)) ;;
    - : Thih.Kind.kind = Kfun (Star, Kfun (Star, Star))

    >>> Kfun(Star, Kfun(Star, Kfun(Star, Star))) ;;
    - : Thih.Kind.kind = Kfun (Star, Kfun (Star, Kfun (Star, Star)))

    >>> Kfun(Kfun(Star, Star), Kfun(Star, Star)) ;;
    - : Thih.Kind.kind = Kfun (Kfun (Star, Star), Kfun (Star, Star))

    >>> Kfun(Star,Kfun(Kfun(Star, Star), Star)) ;;
    - : Thih.Kind.kind = Kfun (Star, Kfun (Kfun (Star, Star), Star))

*)

(* 4 Types *)
module Type = struct
  open Kind
  (* 型変数 *)
  type tyvar = Tyvar of Id.id * kind
  (* 型コンストラクタ *)
  type tycon = Tycon of Id.id * kind
  (* 型 *)
  type type_ =
    | TVar of tyvar
    | TCon of tycon
    | TAp of type_ * type_
    | TGen of int

  let tUnit :type_ = TCon(Tycon("()", Star))
  let tChar :type_ = TCon(Tycon("Char", Star))
  let tInt :type_ = TCon(Tycon("Int", Star))
  let tInteger :type_ = TCon(Tycon("Integer", Star))
  let tFloat :type_ = TCon(Tycon("Float", Star))
  let tDouble :type_ = TCon(Tycon("Double", Star))

  let tList :type_ = TCon(Tycon("[]", Kfun(Star, Star)))
  let tArrow :type_ = TCon(Tycon("(->)", Kfun(Star, Kfun(Star, Star))))
  let tTuple2 :type_ = TCon(Tycon("(,)", Kfun(Star, Kfun(Star, Star))))

  let fn (a:type_) (b:type_) :type_ = TAp(TAp(tArrow, a), b)

  let list t :type_ = TAp(tList, t)

  let tString :type_ = list tChar

  let pair a b :type_ = TAp(TAp(tTuple2, a), b)

  let tyvarKind (Tyvar(_, k)) :kind = k
  let tyconKind (Tycon(_, k)) :kind = k
  let rec typeKind t:kind =
    match t with
    | TCon tc -> tyconKind tc
    | TVar u -> tyvarKind u
    | TAp(t, _) ->
      begin match typeKind t with
        | Kfun(_, k) -> k
        | _ -> failwith "inconsistent type"
      end
    | TGen _ -> failwith "generic type variables have no kind"

  let rec show (t:type_): string =
    begin match t with
      | TVar(Tyvar(id,kind)) -> Printf.sprintf "TVar(Tyvar(%s,%s))" id (Kind.show kind)
      | TCon(Tycon(id,kind)) -> Printf.sprintf "TCon(Tycon(%s,%s))" id (Kind.show kind)
      | TAp(t1,t2)           -> Printf.sprintf "TAp(%s,%s)" (show t1) (show t2)
      | TGen(i)              -> Printf.sprintf "TGen(%d)" i
    end
  let show_list = Pre.show_list show ";"
end

(*|

    >>> open Thih.Type;;
    

テスト用の関数tesを作っておく。

    >>> let tes(t: type_) = (typeKind(t), t);;
    val tes : Thih.Type.type_ -> Thih.Kind.kind * Thih.Type.type_ = <fun>

## パラメータのない型

    >>> tes(tUnit);;
    - : Thih.Kind.kind * Thih.Type.type_ = (Star, TCon (Tycon ("()", Star)))

    >>> tes(tChar);;
    - : Thih.Kind.kind * Thih.Type.type_ = (Star, TCon (Tycon ("Char", Star)))

    >>> tes(tInt);;
    - : Thih.Kind.kind * Thih.Type.type_ = (Star, TCon (Tycon ("Int", Star)))

    >>> tes(tInteger);;
    - : Thih.Kind.kind * Thih.Type.type_ = (Star, TCon (Tycon ("Integer", Star)))

    >>> tes(tFloat);;
    - : Thih.Kind.kind * Thih.Type.type_ = (Star, TCon (Tycon ("Float", Star)))

    >>> tes(tDouble);;
    - : Thih.Kind.kind * Thih.Type.type_ = (Star, TCon (Tycon ("Double", Star)))

## List[T] のようなパラメータが１つある型

    >>> tes(tList);;
    - : Thih.Kind.kind * Thih.Type.type_ = (Kfun (Star, Star), TCon (Tycon ("[]", Kfun (Star, Star))))

## T=>F のようなパラメータが２つある型

    >>> tes(tArrow);;
    - : Thih.Kind.kind * Thih.Type.type_ = (Kfun (Star, Kfun (Star, Star)), TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))))

## カンマもT,Fみたいに２つのパラメータが必要

    >>> tes(tTuple2);;
    - : Thih.Kind.kind * Thih.Type.type_ = (Kfun (Star, Kfun (Star, Star)), TCon (Tycon ("(,)", Kfun (Star, Kfun (Star, Star)))))


## fn関数で２つの型をしていして関数の型を生成出来る

    >>> let fn_int_int = fn(tInt)(tInt);;
    val fn_int_int : Thih.Type.type_ = TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TCon (Tycon ("Int", Star))), TCon (Tycon ("Int", Star)))

TApが2つある。
    
    >>> tes(fn_int_int);;
    - : Thih.Kind.kind * Thih.Type.type_ = (Star, TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TCon (Tycon ("Int", Star))), TCon (Tycon ("Int", Star))))

## １つの型を指定してリスト型を生成できる

    >>> let list_int = list(tInt);;
    val list_int : Thih.Type.type_ = TAp (TCon (Tycon ("[]", Kfun (Star, Star))), TCon (Tycon ("Int", Star)))

TApが1つある。

    >>> tes(list_int);;
    - : Thih.Kind.kind * Thih.Type.type_ = (Star, TAp (TCon (Tycon ("[]", Kfun (Star, Star))), TCon (Tycon ("Int", Star))))

## tStringはCharのリスト型だ。

TApが1つある。

    >>> tes(tString);;
    - : Thih.Kind.kind * Thih.Type.type_ = (Star, TAp (TCon (Tycon ("[]", Kfun (Star, Star))), TCon (Tycon ("Char", Star))))

## ペアは2つの型をもつのでTApが２つある

    >>> let pair_int_char = pair(tInt)(tChar);;
    val pair_int_char : Thih.Type.type_ = TAp (TAp (TCon (Tycon ("(,)", Kfun (Star, Kfun (Star, Star)))), TCon (Tycon ("Int", Star))), TCon (Tycon ("Char", Star)))

    >>> tes(pair_int_char);;
    - : Thih.Kind.kind * Thih.Type.type_ = (Star, TAp (TAp (TCon (Tycon ("(,)", Kfun (Star, Kfun (Star, Star)))), TCon (Tycon ("Int", Star))), TCon (Tycon ("Char", Star))))

    >>> let pair_int = TAp(TCon(Tycon("(,)", Kfun(Star, Kfun(Star, Star)))), TCon(Tycon("Int", Star))) ;;
    val pair_int : Thih.Type.type_ = TAp (TCon (Tycon ("(,)", Kfun (Star, Kfun (Star, Star)))), TCon (Tycon ("Int", Star)))

    >>> tes(pair_int);;
    - : Thih.Kind.kind * Thih.Type.type_ = (Kfun (Star, Star), TAp (TCon (Tycon ("(,)", Kfun (Star, Kfun (Star, Star)))), TCon (Tycon ("Int", Star))))


*)
(* 5 Substitutions *)
module Subst = struct
  open Type

  type subst = (tyvar * type_) list

  let nullSubst : subst = []

  let (+->) u t : subst = [(u, t)]

  (* 型変数を展開する *)
  let rec typeApply (s : subst) (t:type_):type_ = 
    begin match t with
      | TVar u as t ->
        begin try
          List.assoc u s
        with
          Not_found -> t
        end
      | TAp(l, r) -> TAp(typeApply s l, typeApply s r)
      | t -> t
    end

  let rec typeTv (t:type_):tyvar list =
    begin match t with
      | TVar u -> [u]
      | TAp(l, r) -> Pre.union (typeTv l) (typeTv r)
      | _ -> []
    end

  let listApply (apply : subst -> 'a -> 'b) (s : subst) (xs:'a list):'b list =
    List.map (apply s) xs

  let listTv (tv:'a -> tyvar list) (xs:'a list) : tyvar list =
    Pre.nub (List.concat (List.map tv xs))

  let (@@) (s1:subst) (s2 : subst) : subst =
    List.map begin fun (u, t) ->
      (u, typeApply s1 t)
    end s2 @ s1

  let merge s1 s2 : subst =
    let agree =
      let agreeOnVar v =
        typeApply s1 (TVar v) = typeApply s2 (TVar v)
      in
      List.for_all agreeOnVar (Pre.intersect (List.map fst s1) (List.map fst s2))
    in
    if agree
    then s1 @ s2
    else failwith "substitutions do not agree"

  let show (subst:subst):string =
    Pre.show_list begin fun (Tyvar(id,kind),type_) ->
      Printf.sprintf "Tyvar(%s,%s),%s" id (Kind.show kind) (Type.show type_)
    end "; " subst

  let rec show_tyvar(tv:tyvar): string = 
    begin match tv with
      | Tyvar(id,kind) -> Printf.sprintf "Tyvar(%s,%s)" id (Kind.show kind)
    end

  let show_tyvar_list xs :string =
    Pre.show_list begin fun (Tyvar(id,kind)) ->
      Printf.sprintf "Tyvar(%s,%s)" id (Kind.show kind)
    end "; " xs
end
(*|

    >>> open Thih.Subst;;
    

## nullSubst
    
    >>> Subst.nullSubst;;
    - : Thih.Subst.subst = []

## +->

substは+->演算子で作れる

    >>> let subst = Tyvar("a", Star) +-> tInt;;
    val subst : Thih.Subst.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]


    >>> let subst1 = Tyvar("b", Star) +-> tChar ;;
    val subst1 : Thih.Subst.subst = [(Tyvar ("b", Star), TCon (Tycon ("Char", Star)))]

substはリストなので ::: で結ß合出来る

    >>> let subst2 = subst @ subst1 ;;
    val subst2 : (Thih.Type.tyvar * Thih.Type.type_) list = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star))); (Tyvar ("b", Star), TCon (Tycon ("Char", Star)))]

## typeApply

typeApplyはsubstを元に型変数がsubstにあれば置き換える。

    >>> let tva = TVar(Tyvar("a", Star)) ;;
    val tva : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let tvb = TVar(Tyvar("b", Star)) ;;
    val tvb : Thih.Type.type_ = TVar (Tyvar ("b", Star))

    >>> (typeApply(subst)(tva));;
    - : Thih.Type.type_ = TCon (Tycon ("Int", Star))

    >>> (typeApply(subst)(tvb));;
    - : Thih.Type.type_ = TVar (Tyvar ("b", Star))

TApの中身も置き換わる

    >>> let tap = TAp(tva, tvb) ;;
    val tap : Thih.Type.type_ = TAp (TVar (Tyvar ("a", Star)), TVar (Tyvar ("b", Star)))

    >>> typeApply(subst)(tap) ;;
    - : Thih.Type.type_ = TAp (TCon (Tycon ("Int", Star)), TVar (Tyvar ("b", Star)))

    >>> let tap2 = TAp(tva, tva) ;;
    val tap2 : Thih.Type.type_ = TAp (TVar (Tyvar ("a", Star)), TVar (Tyvar ("a", Star)))

    >>> typeApply(subst)(tap2) ;;
    - : Thih.Type.type_ = TAp (TCon (Tycon ("Int", Star)), TCon (Tycon ("Int", Star)))


## typeTv

typeTvでは内部で使っている型変数のリストを返す

    >>> typeTv(tva) ;;
    - : Thih.Type.tyvar list = [Tyvar ("a", Star)]

    >>> typeTv(tvb) ;;
    - : Thih.Type.tyvar list = [Tyvar ("b", Star)]

tapは2つの型を使っているのでaとbが返る

    >>> typeTv(tap) ;;
    - : Thih.Type.tyvar list = [Tyvar ("a", Star); Tyvar ("b", Star)]

## listApply

listApplyは複数の型を受け取って、展開する

    >>> listApply(typeApply)(subst)([tva; tvb]);;
    - : Thih.Type.type_ list = [TCon (Tycon ("Int", Star)); TVar (Tyvar ("b", Star))]

## listTv

listTvはlist全体の内部で使っている型変数を求める

## +-> 2

    >>> let tva = TVar(Tyvar("a", Star)) ;;
    val tva : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let tvb = TVar(Tyvar("b", Star)) ;;
    val tvb : Thih.Type.type_ = TVar (Tyvar ("b", Star))

TApの中身も置き換わる

    >>> let tap = TAp(tva, tvb) ;;
    val tap : Thih.Type.type_ = TAp (TVar (Tyvar ("a", Star)), TVar (Tyvar ("b", Star)))

    >>> listTv(typeTv)([tva; tap]) ;;
    - : Thih.Type.tyvar list = [Tyvar ("a", Star); Tyvar ("b", Star)]

## @@

    >>> let subst = Tyvar("a", Star) +-> tInt ;;
    val subst : Thih.Subst.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> let subst1 = Tyvar("b", Star) +-> TVar(Tyvar("a", Star)) @ Tyvar("a", Star) +-> tChar ;;
    val subst1 : (Thih.Type.tyvar * Thih.Type.type_) list = [(Tyvar ("b", Star), TVar (Tyvar ("a", Star))); (Tyvar ("a", Star), TCon (Tycon ("Char", Star)))]

    >>> let subst2 = Tyvar("b", Star) +-> TVar(Tyvar("a", Star)) @ Tyvar("a", Star) +-> tInt ;;
    val subst2 : (Thih.Type.tyvar * Thih.Type.type_) list = [(Tyvar ("b", Star), TVar (Tyvar ("a", Star))); (Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

@@演算子で2つのsubstを結合出来る。最初のsubstをs2に実行して結合する

    >>> (subst @@ subst1) ;;
    - : Thih.Subst.subst = [(Tyvar ("b", Star), TCon (Tycon ("Int", Star))); (Tyvar ("a", Star), TCon (Tycon ("Char", Star))); (Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> (subst @@ subst2) ;;
    - : Thih.Subst.subst = [(Tyvar ("b", Star), TCon (Tycon ("Int", Star))); (Tyvar ("a", Star), TCon (Tycon ("Int", Star))); (Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

## merge

@@と似ているのだけど、s1とs2でおかしいものがあったらエラーにする

    >>> merge(subst)(subst1) ;;
    Exception: Failure "substitutions do not agree".

    >>> merge(subst)(subst2) ;;
    - : Thih.Subst.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star))); (Tyvar ("b", Star), TVar (Tyvar ("a", Star))); (Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

*)

(* 6 Unification and Matching *)
module Unify = struct
  open List
  open Kind
  open Type
  open Subst
  
  let rec mgu (t1:type_) (t2:type_):subst =
    match t1, t2 with
    | TAp(l, r), TAp(l', r') ->
      let s1 = mgu l l' in
      let s2 = mgu (typeApply s1 r) (typeApply s1 r') in
      s2 @@ s1
    | TVar u, t | t, TVar u -> varBind u t
    | TCon tc1, TCon tc2 when tc1 = tc2 -> nullSubst
    | _ -> failwith "types do not unify"

  and varBind (u:tyvar) (t:type_):subst =
    match t with
    | _ when t = TVar u                -> nullSubst
    | _ when mem u (typeTv t)          -> failwith "occurs check fails"
    | _ when tyvarKind u <> typeKind t -> failwith "kinds do not match"
    | _                                -> u +-> t

  let rec match_ (t1:type_) (t2:type_):subst =
    match t1, t2 with
    | TAp(l, r), TAp(l', r') ->
      let sl = match_ l l' in
      let sr = match_ r r' in
      merge sl sr
    | TVar u, t when tyvarKind u = typeKind t -> u +-> t
    | TCon tc1, TCon tc2 when tc1 = tc2 -> nullSubst
    | _ -> failwith "types do not match"
end
(*|

    >>> open Thih.Unify ;;
    
    
    >>> let t1 = TVar(Tyvar("a", Star)) ;;
    val t1 : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let t2 = TVar(Tyvar("b", Star)) ;;
    val t2 : Thih.Type.type_ = TVar (Tyvar ("b", Star))

    >>> let tv1 = Tyvar("a", Star) ;;
    val tv1 : Thih.Type.tyvar = Tyvar ("a", Star)

    >>> let t3 = tInt ;;
    val t3 : Thih.Type.type_ = TCon (Tycon ("Int", Star))

## mgu

    >>> let subst = mgu(t1)(t2);;
    val subst : Thih.Subst.subst = [(Tyvar ("a", Star), TVar (Tyvar ("b", Star)))]

    >>> subst =
        [(Tyvar("a", Star), TVar(Tyvar("b", Star)))];;
    - : bool = true

    >>> let subst2 = mgu(t1)(t3);;
    val subst2 : Thih.Subst.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> subst2 =
        [(Tyvar("a", Star), tInt)];;
    - : bool = true

## varBind

    >>> let subst = varBind(tv1)(t1);;
    val subst : Thih.Subst.subst = []

    >>> subst = [];;
    - : bool = true

    >>> let subst2 = varBind(tv1)(t3);;
    val subst2 : Thih.Subst.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> subst2 =
        [(Tyvar("a", Star), tInt)];;
    - : bool = true

## match_

    >>> let subst = match_(t1)(t2);;
    val subst : Thih.Subst.subst = [(Tyvar ("a", Star), TVar (Tyvar ("b", Star)))]

    >>> subst =
        [(Tyvar("a", Star), TVar(Tyvar("b", Star)))];;
    - : bool = true

    >>> let subst2 = match_(t1)(t3);;
    val subst2 : Thih.Subst.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> subst2 = [(Tyvar("a", Star), tInt)] ;;
    - : bool = true

*)

(* 7 Type Classes, Predicates and Qualified Types *)
module Pred = struct
  open List
  open Kind
  open Type
  open Subst


  (* 7.1 Basic definitions *)
  type pred = IsIn of Id.id * type_

  let p (IsIn(s, t)) =
    s  ^ " " ^ (Type.show t)

  let ps pred =
    Pre.show_list p ", " pred

  type 't qual = Qual of pred list * 't

  let p_qual q =
    begin match q with
      | Qual(preds,ty) -> ps preds ^ " => " ^ Type.show ty
    end

  let predApply (s:subst) (pred:pred):pred =
    match pred with
    | IsIn(i, t) -> IsIn(i, Subst.typeApply s t)

  let predTv (pred:pred):tyvar list =
    match pred with
    | IsIn(_, t) -> Subst.typeTv t

  let predsApply (s:subst) (xs:pred list):pred list =
    Subst.listApply predApply s xs

  let predsTv (xs:'a list) : tyvar list =
    Subst.listTv predTv xs

  let qualTypeApply (s:subst) (qual:type_ qual):type_ qual =
    match qual with
    | Qual(ps, t) -> Qual(predsApply s ps, Subst.typeApply s t)

  let qualTypeTv qual =
    match qual with
    | Qual(ps, t) ->
      Pre.union (predsTv ps) (Subst.typeTv t)

  let lift (m:type_->type_->'a) (p:pred) (p':pred):'a =
    match (p, p') with
    | IsIn(i, t), IsIn(i', t') ->
      if i = i' then m t t'
      else failwith "classes differ"

  let mguPred = lift Unify.mgu

  let matchPred = lift Unify.match_

  type inst = pred qual

  let p_inst i =
    begin match i with
    | Qual(preds,pred) -> Printf.sprintf "Qual(%s,%s)" (ps preds) (p pred)
    end

  type class_ = Id.id list * inst list

  let (==>) ps p = Qual(ps, p)

  (* 7.2 Class Environments *)

  type classEnv = {
    classes : (Id.id -> class_);
    defaults : type_ list;
  }

  let initialEnv :classEnv = {
    classes = (fun i -> raise Not_found);
    defaults = [tInteger; tDouble]
  }

  let modify (ce:classEnv) i c =
    { ce with classes = fun j -> if i = j then c else ce.classes j; }

  let super (ce:classEnv) i = fst (ce.classes i)

  let insts (ce:classEnv) i = snd (ce.classes i)

  let defined (ce:classEnv) i =
    try
      ignore (ce.classes i);
      true
    with Not_found -> false

  type envTransformer = classEnv -> classEnv

  let addClass i is : envTransformer =
    fun (ce:classEnv) ->
      if defined ce i then failwith "class already defined"
      else if exists (fun i -> not (defined ce i)) is then
        failwith "superclass not defined"
      else modify ce i (is, [])

  let (<:>) (f : envTransformer) (g : envTransformer) : envTransformer =
    fun (ce:classEnv) -> g (f ce)

  let addCoreClasses :envTransformer =
        addClass "Eq" []
    <:> addClass "Ord" ["Eq"]
    <:> addClass "Show" []
    <:> addClass "Read" []
    <:> addClass "Bounded" []
    <:> addClass "Enum" []
    <:> addClass "Functor" []
    <:> addClass "Monad" []

  let addNumClasses :envTransformer =
        addClass "Num" ["Eq"; "Show"]
    <:> addClass "Real" ["Num"; "Ord"]
    <:> addClass "Fractional" ["Num"]
    <:> addClass "Integral" ["Real"; "Enum"]
    <:> addClass "RealFrac" ["Real"; "Fractional"]
    <:> addClass "Floating" ["Fractional"]
    <:> addClass "RealFloat" ["RealFrac"; "Floating"]

  let addPreludeClasses :envTransformer =
    addCoreClasses <:> addNumClasses

  let overlap (p:pred) (q:pred) : bool =
    try
      ignore (mguPred p q);
      true
    with _ -> false

  let addInst ps (IsIn(i, _) as p) : envTransformer =
    fun (ce:classEnv) ->
      if not (defined ce i) then failwith "no class for instance";
      let its = insts ce i in
      let qs = map (fun (Qual(_, q)) -> q) its in
      if exists (overlap p) qs then failwith "overlapping instance";      
      let c = super ce i, Qual(ps, p) :: its in
      modify ce i c

  let exampleInsts : envTransformer =
        addPreludeClasses
    <:> addInst [] (IsIn("Ord", tUnit))
    <:> addInst [] (IsIn("Ord", tChar))
    <:> addInst [] (IsIn("Ord", tInt))
    <:> addInst [IsIn("Ord", TVar(Tyvar("a", Star)));
                 IsIn("Ord", TVar(Tyvar("b", Star)))]
                (IsIn("Ord", pair (TVar(Tyvar("a", Star)))
                                  (TVar(Tyvar("b", Star)))))

  (* 7.3 Entailment *)

  let rec bySuper (ce:classEnv) (IsIn(i, t) as p) =
    p :: concat (map (fun i' -> bySuper ce (IsIn(i', t))) (super ce i))

  let byInst (ce:classEnv) (IsIn(i, t) as p) =
    let tryInst (Qual(ps, h)) =
      try
       let u = matchPred h p in
       Some (map (predApply u) ps)
      with _ -> None in
    let rec msum = function
      | [] -> None
      | None :: xs -> msum xs
      | x :: _ -> x in
    msum (map tryInst (insts ce i))

  let rec entail (ce:classEnv) ps p =
    exists (mem p) (map (bySuper ce) ps) ||
    match byInst ce p with
    | None -> false
    | Some qs -> for_all (entail ce ps) qs

  (* 7.4 Context Reduction *)

  let inHnf (p:pred):bool =
    match p with
    | IsIn(_, t) ->
      let rec hnf = function
        | TVar _ -> true
        | TCon _ -> false
        | TAp(t, _) -> hnf t
        | TGen _ -> failwith "context reduction on generic variable"
      in
      hnf t

  let rec toHnfs (ce:classEnv) ps = concat (map (toHnf ce) ps)
  and toHnf (ce:classEnv) p =
    if inHnf p then [p]
    else
      match byInst ce p with
      | None -> failwith "context reduction"
      | Some ps -> toHnfs ce ps

  let simplify (ce:classEnv) ps =
    let rec loop rs = function
      | [] -> rs
      | p :: ps ->
        if entail ce (rs @ ps) p then loop rs ps
        else loop (p :: rs) ps in
    loop [] ps

  let reduce (ce:classEnv) ps =
    simplify ce (toHnfs ce ps)

  let scEntail (ce:classEnv) ps p =
    exists (mem p) (map (bySuper ce) ps)
end
(*|

    >>> open Thih.Pred;;
    

## pred

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let pred = IsIn("Num", ty) ;;
    val pred : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> pred = IsIn("Num", TVar(Tyvar("a", Star)));;
    - : bool = true

## pred list

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let preds = [IsIn("Num", ty); IsIn("B", ty)] ;;
    val preds : Thih.Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star))); IsIn ("B", TVar (Tyvar ("a", Star)))]

    >>> preds =
        [
          IsIn("Num", TVar(Tyvar("a", Star)));
          IsIn("B", TVar(Tyvar("a", Star)))
        ];;
    - : bool = true

## pred & qual

(Num a) => a -> Int

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let pred = IsIn("Num", ty) ;;
    val pred : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

## Qual

    >>> let q = Qual([pred], fn(ty)(tInt)) ;;
    val q : Thih.Type.type_ Thih.Pred.qual = Qual ([IsIn ("Num", TVar (Tyvar ("a", Star)))], TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TVar (Tyvar ("a", Star))), TCon (Tycon ("Int", Star))))

    >>> let Qual(_, q2) = q ;;
    val q2 : Thih.Type.type_ = TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TVar (Tyvar ("a", Star))), TCon (Tycon ("Int", Star)))

    >>> q2 =
             TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))),
                  TVar (Tyvar ("a", Star))), TCon (Tycon ("Int", Star)));;
    - : bool = true

## predApply

    >>> let subst = [(Tyvar("a", Star), tInt)] ;;
    val subst : (Thih.Type.tyvar * Thih.Type.type_) list = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> let pred = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val pred : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let pred2 = predApply(subst)(pred) ;;
    val pred2 : Thih.Pred.pred = IsIn ("Num", TCon (Tycon ("Int", Star)))

## predTv

    >>> let pred = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val pred : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let tvs = predTv(pred) ;;
    val tvs : Thih.Type.tyvar list = [Tyvar ("a", Star)]

## predsApply

    >>> let subst = [(Tyvar("a", Star), tInt)] ;;
    val subst : (Thih.Type.tyvar * Thih.Type.type_) list = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> let preds = [IsIn("Num", TVar(Tyvar("a", Star)))] ;;
    val preds : Thih.Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]

    >>> let preds2 = predsApply(subst)(preds) ;;
    val preds2 : Thih.Pred.pred list = [IsIn ("Num", TCon (Tycon ("Int", Star)))]

    >>> preds2 = [IsIn("Num", TCon(Tycon("Int", Star)))];;
    - : bool = true

## predsTv

    >>> let preds = [IsIn("Num", TVar(Tyvar("a", Star)))] ;;
    val preds : Thih.Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]

    >>> let tvs = predsTv(preds) ;;
    val tvs : Thih.Type.tyvar list = [Tyvar ("a", Star)]

    >>> tvs = [Tyvar("a", Star)];;
    - : bool = true

## qualTypeApply


    >>> let subst = Tyvar("a", Star) +-> tInt ;;
    val subst : Thih.Subst.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let pred = IsIn("Num", ty) ;;
    val pred : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let qual = Qual([pred], fn(ty)(tInt)) ;;
    val qual : Thih.Type.type_ Thih.Pred.qual = Qual ([IsIn ("Num", TVar (Tyvar ("a", Star)))], TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TVar (Tyvar ("a", Star))), TCon (Tycon ("Int", Star))))

    >>> let qual2 = qualTypeApply(subst)(qual) ;;
    val qual2 : Thih.Type.type_ Thih.Pred.qual = Qual ([IsIn ("Num", TCon (Tycon ("Int", Star)))], TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TCon (Tycon ("Int", Star))), TCon (Tycon ("Int", Star))))

    >>> qual = Qual(
        [
          IsIn("Num", TVar(Tyvar("a", Star)))],
        TAp(TAp(TCon(Tycon("(->)", Kfun(Star, Kfun(Star, Star)))),
          TVar(Tyvar("a", Star))), TCon(Tycon("Int", Star))));;
    - : bool = true

    >>> qual2 = Qual(
        [
          IsIn("Num", TCon(Tycon("Int", Star)))],
        TAp(TAp(TCon(Tycon("(->)", Kfun(Star, Kfun(Star, Star)))),
          TCon(Tycon("Int", Star))), TCon(Tycon("Int", Star))));;
    - : bool = true

## qualTypeTv

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let pred = IsIn("Num", ty) ;;
    val pred : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let qual = Qual([pred], fn(ty)(tInt)) ;;
    val qual : Thih.Type.type_ Thih.Pred.qual = Qual ([IsIn ("Num", TVar (Tyvar ("a", Star)))], TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TVar (Tyvar ("a", Star))), TCon (Tycon ("Int", Star))))

    >>> let tvs = qualTypeTv(qual) ;;
    val tvs : Thih.Type.tyvar list = [Tyvar ("a", Star)]

    >>> tvs = [Tyvar("a", Star)] ;;
    - : bool = true

## mguPred

    >>> let pred1 = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val pred1 : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let pred2 = IsIn("Num", tInt) ;;
    val pred2 : Thih.Pred.pred = IsIn ("Num", TCon (Tycon ("Int", Star)))

    >>> let subst = mguPred(pred1)(pred2) ;;
    val subst : Thih.Subst.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> subst = [(Tyvar("a",Star),TCon(Tycon("Int",Star)))];;
    - : bool = true

    >>> let subst2 = mguPred(pred1)(pred1) ;;
    val subst2 : Thih.Subst.subst = []

## matchPred

    >>> let pred1 = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val pred1 : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let pred2 = IsIn("Num", tInt) ;;
    val pred2 : Thih.Pred.pred = IsIn ("Num", TCon (Tycon ("Int", Star)))

    >>> let subst = matchPred(pred1)(pred2) ;;
    val subst : Thih.Subst.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> subst = [(Tyvar("a", Star), tInt)];;
    - : bool = true

    >>> let subst2 = matchPred(pred1)(pred1) ;;
    val subst2 : Thih.Subst.subst = [(Tyvar ("a", Star), TVar (Tyvar ("a", Star)))]

    >>> subst2 = [(Tyvar("a", Star), TVar(Tyvar("a", Star)))];;
    - : bool = true

## Inst

    >>> let inst = Qual([ IsIn("Ord", tUnit); IsIn("Ord", tChar)], IsIn("Ord", tChar)) ;;
    val inst : Thih.Pred.pred Thih.Pred.qual = Qual ([IsIn ("Ord", TCon (Tycon ("()", Star))); IsIn ("Ord", TCon (Tycon ("Char", Star)))], IsIn ("Ord", TCon (Tycon ("Char", Star))))

    >>> inst = Qual(
        [
          IsIn("Ord", TCon(Tycon("()", Star)));
          IsIn("Ord", TCon(Tycon("Char", Star)))],
        IsIn("Ord", TCon(Tycon("Char", Star))));;
    - : bool = true

## class_ ==>

    >>> let (cls:class_) = (
        ["Eq"],
        [
          [] ==> IsIn("Ord", tUnit);
          [] ==> IsIn("Ord", tChar);
          [] ==> IsIn("Ord",tInt);
          [
            IsIn("Ord",TVar(Tyvar("a", Star)));
            IsIn("Ord",TVar(Tyvar("b", Star)))
          ] ==>
          IsIn("Ord", (pair (TVar(Tyvar("a",Star))) (TVar(Tyvar("b",Star)))))
          
        ]
      ) ;;
    val cls : Thih.Pred.class_ = (["Eq"], [Qual ([], IsIn ("Ord", TCon (Tycon ("()", Star)))); Qual ([], IsIn ("Ord", TCon (Tycon ("Char", Star)))); Qual ([], IsIn ("Ord", TCon (Tycon ("Int", Star)))); Qual ([IsIn ("Ord", TVar (Tyvar ("a", Star))); IsIn ("Ord", TVar (Tyvar ("b", Star)))], IsIn ("Ord", TAp (TAp (TCon (Tycon ("(,)", Kfun (Star, Kfun (Star, Star)))), TVar (Tyvar ("a", Star))), TVar (Tyvar ("b", Star)))))])

# 7.2 Class Environments

## modify

    >>> let ce: classEnv = modify(initialEnv)("ABC")(["A"], [[] ==> IsIn("Ord", tUnit)]) ;;
    val ce : Thih.Pred.classEnv = {classes = <fun>; defaults = [TCon (Tycon ("Integer", Star)); TCon (Tycon ("Double", Star))]}

    >>> ce.defaults = [TCon(Tycon("Integer", Star)); TCon(Tycon("Double", Star))];;
    - : bool = true

## super

    >>> let ce = modify(initialEnv)("ABC")(["A"], [[] ==> IsIn("Ord", tUnit)]) ;;
    val ce : Thih.Pred.classEnv = {classes = <fun>; defaults = [TCon (Tycon ("Integer", Star)); TCon (Tycon ("Double", Star))]}

    >>> let s = super(ce)("ABC") ;;
    val s : Thih.Id.id list = ["A"]

    >>> s = ["A"];;
    - : bool = true

## insts

    >>> let ce = modify(initialEnv)("ABC")(["A"], [[] ==> IsIn("Ord", tUnit)]) ;;
    val ce : Thih.Pred.classEnv = {classes = <fun>; defaults = [TCon (Tycon ("Integer", Star)); TCon (Tycon ("Double", Star))]}

    >>> let s = insts(ce)("ABC") ;;
    val s : Thih.Pred.inst list = [Qual ([], IsIn ("Ord", TCon (Tycon ("()", Star))))]

## defined

    >>> let ce = modify(initialEnv)("ABC")(["A"], [[] ==> IsIn("Ord", tUnit)]) ;;
    val ce : Thih.Pred.classEnv = {classes = <fun>; defaults = [TCon (Tycon ("Integer", Star)); TCon (Tycon ("Double", Star))]}

    >>> let s = defined(ce)("ABC") ;;
    val s : bool = true

## addClass

    >>> let et: envTransformer = addClass("Eq")([]) ;;
    val et : Thih.Pred.envTransformer = <fun>

    >>> let ce = et(initialEnv) ;;
    val ce : Thih.Pred.classEnv = {classes = <fun>; defaults = [TCon (Tycon ("Integer", Star)); TCon (Tycon ("Double", Star))]}

    >>> ce.defaults ;;
    - : Thih.Type.type_ list = [TCon (Tycon ("Integer", Star)); TCon (Tycon ("Double", Star))]

## <:>

    >>> let et1: envTransformer = addClass("Eq")([]) ;;
    val et1 : Thih.Pred.envTransformer = <fun>

    >>> let et2: envTransformer = addClass("Eq2")([]) ;;
    val et2 : Thih.Pred.envTransformer = <fun>

    >>> let et3: envTransformer = et1 <:> et2 ;;
    val et3 : Thih.Pred.envTransformer = <fun>

    >>> (et3 initialEnv).defaults ;;
    - : Thih.Type.type_ list = [TCon (Tycon ("Integer", Star)); TCon (Tycon ("Double", Star))]

    >>> let et4: envTransformer = addClass("Eq")([]) <:> addClass("Eq2")([]) ;;
    val et4 : Thih.Pred.envTransformer = <fun>

    >>> (et4(initialEnv)).defaults ;;
    - : Thih.Type.type_ list = [TCon (Tycon ("Integer", Star)); TCon (Tycon ("Double", Star))]

## overlap

    >>> let pred1 = IsIn("Ord", tUnit) ;;
    val pred1 : Thih.Pred.pred = IsIn ("Ord", TCon (Tycon ("()", Star)))

    >>> let pred2 = IsIn("Ord", tChar) ;;
    val pred2 : Thih.Pred.pred = IsIn ("Ord", TCon (Tycon ("Char", Star)))

    >>> overlap(pred1)(pred2) ;;
    - : bool = false

    >>> overlap(pred1)(pred1) ;;
    - : bool = true

# 7.3 Entailment

## bySuper

    >>> let preds = bySuper(exampleInsts(initialEnv))(IsIn("Num", TVar(Tyvar("a", Star)))) ;;
    val preds : Thih.Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star))); IsIn ("Eq", TVar (Tyvar ("a", Star))); IsIn ("Show", TVar (Tyvar ("a", Star)))]

## byInst

    >>> let preds = byInst(exampleInsts(initialEnv))(IsIn("Num", TVar(Tyvar("a", Star)))) ;;
    val preds : Thih.Pred.pred list option = None

## entail

    >>> let p = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val p : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let ps = [p] ;;
    val ps : Thih.Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]

    >>> let result = entail(exampleInsts(initialEnv))(ps)(p) ;;
    val result : bool = true

# 7.4 Context Reduction

## inHnf

    >>> let r = inHnf(IsIn("Num", TVar(Tyvar("a", Star)))) ;;
    val r : bool = true

    >>> let r2 = inHnf(IsIn("Num", tInt)) ;;
    val r2 : bool = false

## toHnfs

    >>> let preds = [IsIn("Num", TVar(Tyvar("a", Star)))] ;;
    val preds : Thih.Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]

    >>> let preds2 = toHnfs(initialEnv)(preds) ;;
    val preds2 : Thih.Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]

## toHnf

    >>> let pred = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val pred : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let preds = toHnf(initialEnv)(pred) ;;
    val preds : Thih.Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]

## simplify

    >>> let pred = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val pred : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let preds = [pred] ;;
    val preds : Thih.Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]

    >>> let preds2 = simplify(exampleInsts(initialEnv))(preds) ;;
    val preds2 : Thih.Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]

## reduce

    >>> let pred = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val pred : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let preds = [pred] ;;
    val preds : Thih.Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]

    >>> let preds2 = reduce(exampleInsts(initialEnv))(preds) ;;
    val preds2 : Thih.Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]

## scEntail

    >>> let pred = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val pred : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let preds = [pred] ;;
    val preds : Thih.Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]

    >>> let result = scEntail(exampleInsts(initialEnv))(preds)(pred) ;;
    val result : bool = true

*)

(* 8 Type Schemes *)
(*|
    >>> open Thih.Scheme;;
    
*)
module Scheme = struct

  open List
  open Kind
  open Type
  open Pred

  type scheme = Forall of kind list * type_ qual

  let show (Forall(ks, qt):scheme) =
    Printf.sprintf "Forall(%s, %s)" (Kind.show_list ks) (Pred.p_qual qt)

  let schemeApply (s:Subst.subst) (Forall(ks, qt):scheme):scheme =
    Forall(ks, qualTypeApply s qt)

  let schemeTv (Forall(_, qt):scheme):tyvar list = qualTypeTv qt

  let quantify(vs:tyvar list) (qt:type_ qual):scheme =
    let vs' = filter (fun v -> mem v vs) (qualTypeTv qt) in
    let ks = map tyvarKind vs' in
    let newGen v =
      let count = ref 0 in
      let t = TGen !count in
      incr count;
      (v, t) in
    let s = map newGen vs' in
    Forall(ks, qualTypeApply s qt)

  let toScheme (t:type_) :scheme = Forall([], (Qual([], t)))
end
(*|

## scheme

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let pred = IsIn("Num", ty) ;;
    val pred : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let sc = Forall([], Qual([pred], ty)) ;;
    val sc : Thih.Scheme.scheme = Forall ([], Qual ([IsIn ("Num", TVar (Tyvar ("a", Star)))], TVar (Tyvar ("a", Star))))

    >>> sc =
        Forall([],
          Qual(
            [IsIn("Num", TVar(Tyvar("a", Star)))],
            TVar(Tyvar("a", Star))));;
    - : bool = true

## schemeApply
    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let pred = IsIn("Num", ty) ;;
    val pred : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let sc = Forall([], Qual([pred], ty)) ;;
    val sc : Thih.Scheme.scheme = Forall ([], Qual ([IsIn ("Num", TVar (Tyvar ("a", Star)))], TVar (Tyvar ("a", Star))))

    >>> let subst = [(Tyvar("a", Star), tInt)] ;;
    val subst : (Thih.Type.tyvar * Thih.Type.type_) list = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> let sc1 = schemeApply(subst)(sc) ;;
    val sc1 : Thih.Scheme.scheme = Forall ([], Qual ([IsIn ("Num", TCon (Tycon ("Int", Star)))], TCon (Tycon ("Int", Star))))

    >>> sc1 =
        Forall([],
          Qual(
            [IsIn("Num", TCon(Tycon("Int", Star)))],
            TCon(Tycon("Int", Star))));;
    - : bool = true


## schemeTv
    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let pred = IsIn("Num", ty) ;;
    val pred : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let sc = Forall([], Qual([pred], ty)) ;;
    val sc : Thih.Scheme.scheme = Forall ([], Qual ([IsIn ("Num", TVar (Tyvar ("a", Star)))], TVar (Tyvar ("a", Star))))

    >>> let tvs = schemeTv(sc) ;;
    val tvs : Thih.Type.tyvar list = [Tyvar ("a", Star)]

    >>> tvs = [Tyvar("a", Star)];;
    - : bool = true

## quantify
    >>> let tyvar = Tyvar("a", Star) ;;
    val tyvar : Thih.Type.tyvar = Tyvar ("a", Star)

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let pred = IsIn("Num", ty) ;;
    val pred : Thih.Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let qual = Qual([pred], fn(ty)(tInt)) ;;
    val qual : Thih.Type.type_ Thih.Pred.qual = Qual ([IsIn ("Num", TVar (Tyvar ("a", Star)))], TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TVar (Tyvar ("a", Star))), TCon (Tycon ("Int", Star))))

    >>> let sc = quantify([tyvar])(qual) ;;
    val sc : Thih.Scheme.scheme = Forall ([Star], Qual ([IsIn ("Num", TGen 0)], TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TGen 0), TCon (Tycon ("Int", Star)))))

    >>> sc =
        Forall([Star],
          Qual([IsIn("Num", TGen(0))],
            TAp(
              TAp(
                TCon(Tycon("(->)", Kfun(Star, Kfun(Star, Star)))),
                TGen(0)),
              TCon(Tycon("Int", Star)))));;
    - : bool = true

## toScheme

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let sc = toScheme(ty) ;;
    val sc : Thih.Scheme.scheme = Forall ([], Qual ([], TVar (Tyvar ("a", Star))))

    >>> sc = Forall([], Qual([], TVar(Tyvar("a", Star))));;
    - : bool = true

*)
(* 9 Assumptions *)
(*|
    >>> open Thih.Assump;;
    
*)
module Assump = struct

  open Scheme
  open Type
  open Kind
  
  type assump = Assump of Id.id * scheme

  let show (Assump(i, sc):assump) : string =
    Printf.sprintf "Assump(%s, %s)" i (Scheme.show sc)

  let show_list (assumps: assump list) : string =
    Pre.show_list show ";" assumps

  let assumpApply (s:Subst.subst) (Assump(i, sc):assump) : assump =
    Assump(i, schemeApply s sc)

  let assumpTv (Assump(_, sc):assump):Type.tyvar list =
    schemeTv sc

  let assumpsApply (s:Subst.subst) (ass:assump list): assump list =
    Subst.listApply assumpApply s ass

  let assumpsTv (ass:assump list): Type.tyvar list =
    Subst.listTv assumpTv ass

  let find (i:Id.id) (ass:assump list): scheme =
    let Assump(_, sc) = List.find begin fun (Assump(i', _)) ->
      i = i'
    end ass in
    sc
end
(*|
## assump

    >>> let t = TVar(Tyvar("a", Star)) ;;
    val t : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let assump = Assump("ABC", Forall([], Qual([], t))) ;;
    val assump : Thih.Assump.assump = Assump ("ABC", Forall ([], Qual ([], TVar (Tyvar ("a", Star)))))

    >>> assump = Assump("ABC", Forall([], Qual([], TVar(Tyvar("a", Star)))));;
    - : bool = true

## assumpApply

    >>> let t = TVar(Tyvar("a", Star)) ;;
    val t : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let assump = Assump("ABC", Forall([], Qual([], t))) ;;
    val assump : Thih.Assump.assump = Assump ("ABC", Forall ([], Qual ([], TVar (Tyvar ("a", Star)))))

    >>> let subst: subst = [(Tyvar("a", Star), tInt)] ;;
    val subst : Thih.Subst.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> let assump2 = assumpApply(subst)(assump) ;;
    val assump2 : Thih.Assump.assump = Assump ("ABC", Forall ([], Qual ([], TCon (Tycon ("Int", Star)))))

    >>> assump2 = Assump("ABC", Forall([], Qual([], TCon(Tycon("Int", Star)))));;
    - : bool = true

## assumpTv

    >>> let t = TVar(Tyvar("a", Star)) ;;
    val t : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let assump = Assump("ABC", Forall([], Qual([], t))) ;;
    val assump : Thih.Assump.assump = Assump ("ABC", Forall ([], Qual ([], TVar (Tyvar ("a", Star)))))

    >>> let tvs = assumpTv(assump) ;;
    val tvs : Thih.Type.tyvar list = [Tyvar ("a", Star)]
    >>> tvs = [Tyvar("a", Star)] ;;
    - : bool = true

## assumpsApply

    >>> let t = TVar(Tyvar("a", Star)) ;;
    val t : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let assump = Assump("ABC", Forall([], Qual([], t))) ;;
    val assump : Thih.Assump.assump = Assump ("ABC", Forall ([], Qual ([], TVar (Tyvar ("a", Star)))))

    >>> let subst = [(Tyvar("a", Star), tInt)] ;;
    val subst : (Thih.Type.tyvar * Thih.Type.type_) list = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> let assumps = assumpsApply(subst)([assump]) ;;
    val assumps : Thih.Assump.assump list = [Assump ("ABC", Forall ([], Qual ([], TCon (Tycon ("Int", Star)))))]

    >>> assumps =
        [
          Assump("ABC",
            Forall([], Qual([], TCon(Tycon("Int", Star)))))];;
    - : bool = true

## assumpsTv

    >>> let t = TVar(Tyvar("a", Star)) ;;
    val t : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let assump = Assump("ABC", Forall([], Qual([], t))) ;;
    val assump : Thih.Assump.assump = Assump ("ABC", Forall ([], Qual ([], TVar (Tyvar ("a", Star)))))

    >>> let tvs = assumpsTv([assump]) ;;
    val tvs : Thih.Type.tyvar list = [Tyvar ("a", Star)]

    >>> tvs = [Tyvar("a", Star)] ;;
    - : bool = true

## find

    >>> let t = TVar(Tyvar("a", Star)) ;;
    val t : Thih.Type.type_ = TVar (Tyvar ("a", Star))

    >>> let assump = Assump("ABC", Forall([], Qual([], t))) ;;
    val assump : Thih.Assump.assump = Assump ("ABC", Forall ([], Qual ([], TVar (Tyvar ("a", Star)))))

    >>> let assump2 = Assump("ABC2", Forall([], Qual([], tInt))) ;;
    val assump2 : Thih.Assump.assump = Assump ("ABC2", Forall ([], Qual ([], TCon (Tycon ("Int", Star)))))

    >>> let sc = find("ABC")([assump;assump2]) ;;
    val sc : Thih.Scheme.scheme = Forall ([], Qual ([], TVar (Tyvar ("a", Star))))

    >>> sc = Forall([], Qual([], TVar(Tyvar("a", Star))));;
    - : bool = true

*)

(* 10 A Type Inference Monad *)
(*|
    >>> open Thih.TIMonad;;
    
*)
module TIMonad = struct

  open Kind
  open Type
  open Subst
  open Pred
  open Scheme
  open List

  type ti = subst ref * int ref

  let show (({contents=subst},{contents=i}):ti) : string =
    Printf.sprintf "({contents=%s}, {contents=%d})" (Subst.show subst) i

  let runTI (f : ti -> 'a):'a =
    f (ref nullSubst, ref 0)

  let getSubst ((s, _) : ti):subst = !s

  let extSubst ((s, _) : ti) (u:subst) :unit = s := u @@ !s

  let unify (ti:ti) (t1:type_) (t2:type_) :unit=
    let s:subst = getSubst ti in
    let u = Unify.mgu (typeApply s t1) (typeApply s t2) in
    extSubst ti u

  let newTVar ((_, n) : ti) k : type_ =
    let v = Tyvar(Id.enumId !n, k) in
    incr n;
    TVar v

  let rec typeInst (ts:type_ list) (t:type_):type_ = 
    begin match t with
      | TAp(l, r) -> TAp(typeInst ts l, typeInst ts r)
      | TGen n -> nth ts n
      | t -> t
    end

  let listInst (inst: type_ list -> 'a -> 'a)
    (ts : type_ list) (xs : 'a list) : 'a list =
    map (inst ts) xs

  let predInst (ts: type_ list) (IsIn(c, t): pred):pred =
    IsIn(c, typeInst ts t)

  let qualTypeInst (ts:type_ list)
    (Qual(ps, t):type_ qual):type_ qual =
    Qual(listInst predInst ts ps, typeInst ts t)

  let freshInst (ti:ti) (Forall(ks, qt):scheme) : type_ qual =
    let ts = map (newTVar ti) ks in
    qualTypeInst ts qt
end
(*|

## ti

    >>> let subst = [(Tyvar("a", Star), tInt)] ;;
    val subst : (Thih.Type.tyvar * Thih.Type.type_) list = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> let ti = (ref subst, ref 1) ;;
    val ti : (Thih.Type.tyvar * Thih.Type.type_) list ref * int ref = ({contents = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]}, {contents = 1})

    >>> ti =
      ({contents=[(Tyvar("a", Star), TCon(Tycon("Int", Star)))]}, {contents=1});;
    - : bool = true

## runTI

    >>> let n = runTI begin fun (subst, n) ->
      n := !n + 1;
      !n
    end ;;
    val n : int = 1

    >>> n = 1;;
    - : bool = true

## getSubst

    >>> runTI begin fun ti ->
      let subst = getSubst(ti) in
      subst = []
    end;;
    - : bool = true

## extSubst

    >>> runTI begin fun ti ->
        let subst = [(Tyvar("a", Star), tInt)] in
        extSubst(ti)(subst);
        let subst2 = getSubst(ti) in

        subst2 = [(Tyvar("a", Star), TCon(Tycon("Int", Star)))]
      end;;
    - : bool = true

## unify

    >>> runTI begin fun ti ->
        let t1 = TVar(Tyvar("a", Star)) in
        unify(ti)(t1)(tInt);

        t1 = TVar(Tyvar("a", Star))
      end;;
    - : bool = true

## newTVar

    >>> runTI begin fun ti ->
        let t1 = newTVar(ti)(Star) in
        unify(ti)(t1)(tInt);
        let t2:type_ = typeApply(getSubst(ti))(t1) in
        (t1 = TVar(Tyvar("v0", Star)), t2 = tInt)
      end ;;
    - : bool * bool = (true, true)

## freshInst

    >>> runTI begin fun ti ->
        let ty = TVar(Tyvar("a", Star)) in
        let sc = toScheme(ty) in

        let tq:type_ qual = freshInst(ti)(sc) in

        (sc = Forall([], Qual([], TVar(Tyvar("a", Star)))),
        tq = Qual([], TVar(Tyvar("a", Star))))
      end ;;
    - : bool * bool = (true, true)

*)

(* 11 Type Inference *)
(*|
    >>> open Thih.Infer;;
    
*)
module Infer = struct
  open Pred
  open Assump
  open TIMonad

  type ('e, 't) infer = ti -> classEnv -> assump list -> 'e -> pred list * 't
end
(*|

*)

(* 11.1 Literals *)
(*|
    >>> open Thih.Lit;;
    
*)
module Lit = struct
  open Kind
  open Type
  open Pred
  open TIMonad
  open Infer
  open Big_int
  open Num
  type literal =
    | LitInt of big_int
    | LitChar of char
    | LitRat of num
    | LitStr of string

  let tiLit (ti:ti) (lit:literal):pred list * type_ =
    begin match lit with
      | LitChar _ -> ([], tChar)
      | LitInt _ ->
        let v = newTVar ti Star in
        ([IsIn("Num", v)], v)
      | LitStr _ -> ([], tString)
      | LitRat _ ->
        let v = newTVar ti Star in
        ([IsIn("Fractional", v)], v)
    end
end
(*|
    >>> #load "nums.cma" ;;
    
    
    >>> open Big_int ;;
    
    
    >>> big_int_of_string "123";;
    - : Big_int.big_int = <abstr>

    
    >>> runTI begin fun ti ->
        let lit = LitInt (big_int_of_string "123") in
        let (preds, ty) = tiLit(ti)(lit) in
        let subst = getSubst(ti) in
        let ty2 = Subst.typeApply(subst)(ty) in

        (preds = [IsIn("Num", TVar(Tyvar("v0", Star)))],
        ty = TVar(Tyvar("v0", Star)),
        ty2 = ty,
        subst = []) = (true, true, true, true)

      end;;
    - : bool = true

*)


(* 11.2 Patterns *)
(*|
    >>> open Thih.Pat;;
    
*)
module Pat = struct
  open Big_int
  open List
  open Kind
  open Type
  open Pred
  open Scheme
  open Assump
  open TIMonad
  open Infer
  open Lit
 
  type pat =
    | PVar of Id.id
    | PWildcard
    | PAs of Id.id * pat
    | PLit of literal
    | PNpk of Id.id * big_int
    | PCon of assump * pat list

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

  and tiPats (ti:ti) (pats:pat list):pred list * assump list * type_ list =
    let (pss, ass, ts) = Pre.split3 (map (tiPat ti) pats) in
    (concat pss, concat ass, ts)
end
(*|
## tiPat PVar

    >>> runTI begin fun ti ->
        let pat = PVar("a") in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        (preds = [],
        assumps =
          [
            Assump("a",Forall([],Qual([],TVar(Tyvar("v0",Star)))))],

        ty = TVar(Tyvar("v0", Star)))
      end
    ;;
    - : bool * bool * bool = (true, true, true)

## tiPat PWildcard

    >>> runTI begin fun ti ->
        let pat = PWildcard in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        (preds = [],
        assumps = [],
        ty = TVar(Tyvar("v0", Star)))
      end
    ;;
    - : bool * bool * bool = (true, true, true)

## tiPat PAs

    >>> runTI begin fun ti ->
        let pat = PAs("a", PWildcard) in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        (preds = [],
        assumps =
          [
            Assump("a",Forall([],Qual([],TVar(Tyvar("v0",Star)))))],

        ty = TVar(Tyvar("v0", Star))
        )
      end
    ;;
    - : bool * bool * bool = (true, true, true)

## tiPat PLit

    >>> runTI begin fun ti ->
        let pat = PLit(LitInt(big_int_of_string "123")) in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        (preds =
          [IsIn("Num",TVar(Tyvar("v0",Star)))],
        assumps = [],
        ty = TVar(Tyvar("v0", Star))
        )
      end
    ;;
    - : bool * bool * bool = (true, true, true)

## tiPat PNpk

    >>> runTI begin fun ti ->
        let pat = PNpk("a",big_int_of_string "10") in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        (preds =
          [IsIn("Integral",TVar(Tyvar("v0",Star)))],
        assumps =
          [
            Assump("a",Forall([],Qual([],TVar(Tyvar("v0",Star)))))],

        ty = TVar(Tyvar("v0", Star))
        )
      end
    ;;
    - : bool * bool * bool = (true, true, true)

## tiPat PCon

    >>> runTI begin fun ti ->
        let t = TVar(Tyvar("a", Star)) in
        let assump = Assump("ABC", Forall([], Qual([], t))) in

        let pat = PCon(assump,[]) in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        (preds = [],
        assumps = [],
        ty = TVar(Tyvar("v0", Star))
        )

      end
    ;;
    - : bool * bool * bool = (true, true, true)

*)

(* 11.3 Expressions
 * 11.4 Alternatives
 * 11.5 From Types to Type Schemes
 * 11.6 Binding Groups *)
(*|
    >>> open Thih.TIMain;;
    
*)
module TIMain = struct
  open List
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

  type ambiguity = tyvar * pred list

  let show_amb ((tv,preds):ambiguity) =
    Printf.sprintf "ambibuity(%s, %s)" (Subst.show_tyvar tv) (Pred.ps preds)

  let show_ambs ambs =
    Pre.show_list show_amb ";" ambs

  let ambiguities (vs:tyvar list) (ps:pred list) : ambiguity list =
    let vs' = Pre.diff (predsTv ps) vs in
    map begin fun v ->
      (v, filter begin fun p ->
        mem v (predTv p)
      end ps)
    end vs'

  let numClasses : Id.id list = [
    "Num"; "Integral"; "Floating"; "Fractional"; "Real"; "RealFloat";
    "RealFrac"]

  let stdClasses : Id.id list = [
    "Eq"; "Ord"; "Show"; "Read"; "Bounded"; "Enum"; "Ix"; "Functor"; "Monad";
    "MonadPlus"] @ numClasses

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

  let withDefaults (f:ambiguity list -> type_ list -> 'a)
    (ce:classEnv) (vs:tyvar list) (ps:pred list):'a =
    let vps = ambiguities vs ps in
    let tss = map (candidates ce) vps in
    if exists Pre.isEmpty tss then failwith "cannot resolve ambiguity"
    else f vps (map hd tss)

  let defaultedPreds (ce:classEnv) (vs:tyvar list) (ps:pred list):pred list =
    withDefaults (fun vps ts -> concat (map snd vps)) ce vs ps

  let defaultSubst (ce:classEnv) (vs:tyvar list) (ps:pred list): subst =
    withDefaults (fun vps ts -> combine (map fst vps) ts) ce vs ps

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

  type expr =
    | Var of Id.id
    | Lit of literal
    | Const of assump
    | Ap of expr * expr
    | Let of bindGroup * expr
    (* | Lam of alt*)
    (* | If of expr * expr * expr*)
    (* | Case of expr * (Pat * Expr) list*)
  and alt = pat list * expr
  and expl = Id.id * scheme * alt list
  and impl = Id.id * alt list
  and bindGroup = expl list * impl list list
  let restricted (bs : impl list):bool =
    let simple (i, alts) = exists begin fun alt ->
      Pre.isEmpty (fst alt)
    end alts in
    exists simple bs

  let rec tiSeq (f : ('bg, assump list) infer) : ('bg list, assump list) infer =
    fun ti ce as_ ->
      begin function
        | [] -> ([], [])
        | bs :: bss ->
          let (ps, as') = f ti ce as_ bs in
          let (qs, as'') = tiSeq f ti ce (as' @ as_) bss in
          (ps @ qs, as'' @ as')
      end

  let rec tiExpr (ti:ti)(ce:classEnv)(as_:assump list)(expr: expr): pred list * type_ =
    begin match expr with
      | Var i ->
        let sc = find i as_ in
        let Qual(ps, t) = freshInst ti sc in
        (ps, t)
      | Const(Assump(_, sc)) ->
        let Qual(ps, t) = freshInst ti sc in
        (ps, t)
      | Lit l -> tiLit ti l
      | Ap(e, f) ->
        let (ps, te) = tiExpr ti ce as_ e in
        let (qs, tf) = tiExpr ti ce as_ f in
        let t = newTVar ti Star in
        unify ti (fn tf t) te;
        (ps @ qs, t)
      | Let(bg, e) ->
        let (ps, as') = tiBindGroup ti ce as_ bg in
        let (qs, t) = tiExpr ti ce (as' @ as_) e in
        (ps @ qs, t)
      (* | Lam(alt) -> tiAlt ti ce as_ alt *)
      (* | If(e, e1, e2) ->
        let (ps,t) = tiExpr ti ce as_ e in
        unify ti t tBool;
        let (ps1,t1) = tiExpr ti ce as_ e1 in
        let (ps2,t2) = tiExpr ti ce as_ e2 in
        unify ti t1 t2;
        (ps @ ps1 @ ps2, t1)*)
      (* | Case(e, branches) ->
        let (ps, t) = tiExpr ti ce as_ e in
        let v = newTVar Star in
        let tiBr (pat, f) =
          let (ps, _as',t') = tiPat pat in
          unify t t';
          let (qs, t'') = tiExpr ce (_as' @ _as) f in
          unify v t'';
          (ps @ qs)
        in
        let pss = mapM tiBr branches in
        (ps @ concat pss, v)
      *)
    end
  and tiAlt : (alt, type_) infer =
    begin fun ti ce as_ (pats, e) ->
      let (ps, as', ts) = tiPats ti pats in
      let (qs, t) = tiExpr ti ce (as' @ as_) e in
      (ps @ qs, fold_right fn ts t)
    end
  and tiAlts (ti:ti)(ce:classEnv)(as_:assump list)(alts:alt list)(t:type_):pred list =
    let (ps, ts) = List.split (map (tiAlt ti ce as_) alts) in
    iter (unify ti t) ts;
    concat ps
  and tiExpl (ti:ti)(ce:classEnv)(as_:assump list)((i, sc, alts) : expl):pred list =
    let Qual(qs, t) = freshInst ti sc in
    let ps = tiAlts ti ce as_ alts t in
    let s = getSubst ti in
    let qs' = predsApply s qs in
    let t' = typeApply s t in
    let fs = assumpsTv (assumpsApply s as_) in
    let gs = Pre.diff (typeTv t') fs in
    let sc' = quantify gs (Qual(qs', t')) in
    let ps' = filter (fun p -> not (entail ce qs' p)) (predsApply s ps) in
    let (ds, rs) = split ce fs gs ps' in
    if sc <> sc' then failwith "signature too general"
    else if not (Pre.isEmpty rs) then failwith "context too weak"
    else ds
  and tiImpls : (impl list, assump list) infer =
    begin fun ti ce as_ bs ->
      let ((bs),is,ts',gs,ds,rs) =
        let ((ce, bs), is, ps',ts',fs) =
          let ts = map (fun _ -> newTVar ti Star) bs in
          let (is, altss) = List.split bs in
          let scs = map toScheme ts in
          let as' = map2 (fun i sc -> Assump(i, sc)) is scs @ as_ in
          let pss = map2 (tiAlts ti ce as') altss ts in
          let s = getSubst ti in
          let ps' = map (predApply s) (concat pss) in
          let ts' = map (typeApply s) ts in
          let fs = assumpsTv (assumpsApply s as_) in
          ((ce, bs), is, ps',ts',fs)
        in
          Printf.printf "kore1\n";
        let vss = map typeTv ts' in
          Printf.printf "kore2\n";
        let gs = Pre.diff (Pre.fold_left1 Pre.union vss) fs in
          Printf.printf "kore3\n";
        let (ds, rs) = split ce fs (Pre.fold_left1 Pre.intersect vss) ps' in
          Printf.printf "kore4\n";
        ((bs), is, ts',gs,ds,rs)
      in
      if restricted bs then
        let gs' = Pre.diff gs (predsTv rs) in
        let scs' = map (fun t -> quantify gs' (Qual([], t))) ts' in
        (ds @ rs, map2 (fun i sc -> Assump(i, sc)) is scs')
      else
        let scs' = map (fun t -> quantify gs (Qual(rs, t))) ts' in
        (ds, map2 (fun i sc -> Assump(i, sc)) is scs')
    end
  and tiBindGroup : (bindGroup, assump list) infer =
    begin fun ti ce as_ (es, iss) ->
      let as' = map (fun (v, sc, _) -> Assump(v, sc)) es in
      let (ps, as'') = tiSeq tiImpls ti ce (as' @ as_) iss in
      let qss = map (tiExpl ti ce (as'' @ as' @ as_)) es in
      (ps @ concat qss, as'' @ as')
    end

  type program = bindGroup list

  let tiProgram (ce:classEnv) (as_:assump list) (bgs : program):assump list =
    runTI begin fun ti ->
      let (ps, as2) = tiSeq tiBindGroup ti ce as_ bgs in
      let s = getSubst ti in
      let rs = reduce ce (predsApply s ps) in
      let s' = defaultSubst ce [] rs in
      assumpsApply (s' @@ s) as2
    end
end
(*|

## mbiguities

    >>>
      let tvs = [Tyvar("a", Star)] in
      let preds = [IsIn("Num", tInt); IsIn("B", tInt)] in
      ambiguities(tvs)(preds)
    ;;
    - : Thih.TIMain.ambiguity list = []

## numClasses

    >>> numClasses ;;
    - : Thih.Id.id list = ["Num"; "Integral"; "Floating"; "Fractional"; "Real"; "RealFloat"; "RealFrac"]

    >>> List.length numClasses ;;
    - : int = 7

## stdClasses

    >>>
      stdClasses =
        ["Eq"; "Ord"; "Show"; "Read"; "Bounded"; "Enum";
          "Ix"; "Functor"; "Monad"; "MonadPlus"; "Num"; "Integral";
          "Floating"; "Fractional"; "Real"; "RealFloat"; "RealFrac"];
    ;;
    - : bool = true

    >>>
      List.length stdClasses = 17
    ;;
    - : bool = true

## candidates

    >>>
      let tv = Tyvar("a", Star) in
      let preds = [IsIn("Num", tInt); IsIn("B", tInt)] in
      Printf.printf("a ----\n");
      let _ = (tv, preds) in
      Printf.printf("b ----\n");
      let ce = addNumClasses(initialEnv) in
      Printf.printf("c ----\n");
      let amb = (Tyvar("B", Star),preds) in
      let ts = candidates(ce)(amb) in
      Printf.printf "ts = %s\n" (Type.show_list ts);
      
      1 = 1
    ;;
    a ---- b ---- Exception: Failure "superclass not defined".

## withDefaults

    >>>
      1 = 1
    ;;
    - : bool = true

## defaultedPreds

    >>>
      1 = 1
    ;;
    - : bool = true

## defaultSubst

    >>>
      1 = 1
    ;;
    - : bool = true

## split

    >>>
      1 = 1
    ;;
    - : bool = true

## restricted

    >>>
      1 = 1
    ;;
    - : bool = true

## tiSeq

    >>>
      1 = 1
    ;;
    - : bool = true

## tiExpr LitStr

    >>>
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (expr: expr) = Lit(LitStr "test") in
        let result:(pred list * type_) =
          tiExpr (ti:ti)(ce:classEnv)(as_:assump list)(expr: expr)
        in
        let expected = ([],tString) in
        expected = result
      end
    ;;
    - : bool = true

## tiExpr LitChar

    >>>
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (expr: expr) = Lit(LitChar 't') in
        let result:(pred list * type_) =
          tiExpr (ti:ti)(ce:classEnv)(as_:assump list)(expr: expr)
        in
        let expected = ([],tChar) in
        expected = result
      end
    ;;
    - : bool = true

## tiAlt LitChar

    >>>
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (expr: expr) = Lit(LitChar 't') in
        let (alt: alt) = ([],expr) in
        let result:(pred list * type_) =
          tiAlt (ti:ti)(ce:classEnv)(as_:assump list)(alt:alt)
        in
        let expected = ([],tChar) in
        expected = result
      end
    ;;
    - : bool = true

## tiAlts LitChar

    >>>
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (expr: expr) = Lit(LitChar 't') in
        let (alt: alt) = ([],expr) in
        let (alts: alt list) = [alt] in
        tiAlts (ti:ti)(ce:classEnv)(as_:assump list)(alts:alt list)(tChar)
      end
    ;;
    - : Thih.Pred.pred list = []

## tiExpl LitChar

    >>>
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (expr: expr) = Lit(LitChar 't') in
        let (alt: alt) = ([],expr) in
        let (alts: alt list) = [alt] in
        let (expl:expl) = ("a",Forall([], Qual([], tChar)), alts) in
        tiExpl (ti:ti)(ce:classEnv)(as_:assump list)(expl)
      end
    ;;
    - : Thih.Pred.pred list = []

## tiImpls null

    >>>
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (impls:impl list) = [] in
        tiImpls (ti:ti)(ce:classEnv)(as_:assump list)(impls)
      end
    ;;
    kore1 kore2 Exception: Invalid_argument "empty list".

## tiBindingGroup

    >>>
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (impls:impl list) = [] in
        let (expr: expr) = Lit(LitChar 't') in
        let (alt: alt) = ([],expr) in
        let (alts: alt list) = [alt] in
        let (expl:expl) = ("a",Forall([], Qual([], tChar)), alts) in
        let (bindGroup:bindGroup) = ([expl], [impls]) in
        let result:(pred list * assump list) =
          tiBindGroup (ti:ti)(ce:classEnv)(as_:assump list)(bindGroup)
        in
        let expected = ([],[]) in
        (expected = result, result)
      end
    ;;
    kore1 kore2 Exception: Invalid_argument "empty list".

## tiProgram

    >>>
      let (ce:classEnv) = Pred.initialEnv in
      let (as_:assump list) = [] in
      let (impls:impl list) = [] in
      let (expr: expr) = Lit(LitChar 't') in
      let (alt: alt) = ([],expr) in
      let (alts: alt list) = [alt] in
      let (expl:expl) = ("a",Forall([], Qual([], tChar)), alts) in
      let (bindGroup:bindGroup) = ([expl], [impls]) in
      let (program:program) = [bindGroup] in
      let result:assump list =
        tiProgram (ce:classEnv)(as_:assump list)(program)
      in
      let expected = [] in
      expected = result
    ;;
    kore1 kore2 Exception: Invalid_argument "empty list".

*)


