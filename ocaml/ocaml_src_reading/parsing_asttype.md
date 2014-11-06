# parsing/asttype.mli

## constant

    type constant =
        Const_int of int
      | Const_char of char
      | Const_string of string * string option
      | Const_float of string
      | Const_int32 of int32
      | Const_int64 of int64
      | Const_nativeint of nativeint

## rec_flag

    type rec_flag = Nonrecursive | Recursive

let recのrecが付いているかフラグ Recursiveだとrecが付いており再帰的です。

## direction_flag

    type direction_flag = Upto | Downto

方向みたいだけど、謎

## private_flag

    type private_flag = Private | Public

object内のメソッドのアクセス属性

## mutable_flag

    type mutable_flag = Immutable | Mutable

object内のvalのmutable属性

## virtual_flag

    type virtual_flag = Virtual | Concrete

object内のvirutal属性

## override_flag

    type override_flag = Override | Fresh

object内のoverride属性

## closed_flag

    type closed_flag = Closed | Open

何かのとじフラグ

## label

    type label = string

何かのラベル

## loc

    type 'a loc = 'a Location.loc = {
      txt : 'a;
      loc : Location.t;
    }

位置情報。txtには色々入れる事が可能

## variance

    type variance =
      | Covariant
      | Contravariant
      | Invariant

謎
