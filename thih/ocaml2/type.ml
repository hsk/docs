(* 4 Types *)
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


(*|

# Type

    >>> open Type;;

テスト用の関数tesを作っておく。

    >>> let tes(t: type_) = (typeKind(t), t);;
    val tes : Type.type_ -> Kind.kind * Type.type_ = <fun>

## パラメータのない型

    >>> tes(tUnit);;
    - : Kind.kind * Type.type_ = (Star, TCon (Tycon ("()", Star)))

    >>> tes(tChar);;
    - : Kind.kind * Type.type_ = (Star, TCon (Tycon ("Char", Star)))

    >>> tes(tInt);;
    - : Kind.kind * Type.type_ = (Star, TCon (Tycon ("Int", Star)))

    >>> tes(tInteger);;
    - : Kind.kind * Type.type_ = (Star, TCon (Tycon ("Integer", Star)))

    >>> tes(tFloat);;
    - : Kind.kind * Type.type_ = (Star, TCon (Tycon ("Float", Star)))

    >>> tes(tDouble);;
    - : Kind.kind * Type.type_ = (Star, TCon (Tycon ("Double", Star)))

## List[T] のようなパラメータが１つある型

    >>> tes(tList);;
    - : Kind.kind * Type.type_ = (Kfun (Star, Star), TCon (Tycon ("[]", Kfun (Star, Star))))

## T=>F のようなパラメータが２つある型

    >>> tes(tArrow);;
    - : Kind.kind * Type.type_ = (Kfun (Star, Kfun (Star, Star)), TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))))

## カンマもT,Fみたいに２つのパラメータが必要

    >>> tes(tTuple2);;
    - : Kind.kind * Type.type_ = (Kfun (Star, Kfun (Star, Star)), TCon (Tycon ("(,)", Kfun (Star, Kfun (Star, Star)))))

## fn関数で２つの型をしていして関数の型を生成出来る

    >>> let fn_int_int = fn(tInt)(tInt);;
    val fn_int_int : Type.type_ = TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TCon (Tycon ("Int", Star))), TCon (Tycon ("Int", Star)))

TApが2つある。
    
    >>> tes(fn_int_int);;
    - : Kind.kind * Type.type_ = (Star, TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TCon (Tycon ("Int", Star))), TCon (Tycon ("Int", Star))))

## １つの型を指定してリスト型を生成できる

    >>> let list_int = list(tInt);;
    val list_int : Type.type_ = TAp (TCon (Tycon ("[]", Kfun (Star, Star))), TCon (Tycon ("Int", Star)))

TApが1つある。

    >>> tes(list_int);;
    - : Kind.kind * Type.type_ = (Star, TAp (TCon (Tycon ("[]", Kfun (Star, Star))), TCon (Tycon ("Int", Star))))

## tStringはCharのリスト型だ。

TApが1つある。

    >>> tes(tString);;
    - : Kind.kind * Type.type_ = (Star, TAp (TCon (Tycon ("[]", Kfun (Star, Star))), TCon (Tycon ("Char", Star))))

## ペアは2つの型をもつのでTApが２つある

    >>> let pair_int_char = pair(tInt)(tChar);;
    val pair_int_char : Type.type_ = TAp (TAp (TCon (Tycon ("(,)", Kfun (Star, Kfun (Star, Star)))), TCon (Tycon ("Int", Star))), TCon (Tycon ("Char", Star)))

    >>> tes(pair_int_char);;
    - : Kind.kind * Type.type_ = (Star, TAp (TAp (TCon (Tycon ("(,)", Kfun (Star, Kfun (Star, Star)))), TCon (Tycon ("Int", Star))), TCon (Tycon ("Char", Star))))

    >>> let pair_int = TAp(TCon(Tycon("(,)", Kfun(Star, Kfun(Star, Star)))), TCon(Tycon("Int", Star))) ;;
    val pair_int : Type.type_ = TAp (TCon (Tycon ("(,)", Kfun (Star, Kfun (Star, Star)))), TCon (Tycon ("Int", Star)))

    >>> tes(pair_int);;
    - : Kind.kind * Type.type_ = (Kfun (Star, Star), TAp (TCon (Tycon ("(,)", Kfun (Star, Kfun (Star, Star)))), TCon (Tycon ("Int", Star))))

*)
