-module(t04_type_tests).
-include_lib("eunit/include/eunit.hrl").

tes(Type, Kind, Type2) ->
  Kind = type:typeKind(Type),
  Type = Type2.

test1_test() ->

  % パラメータのない型
  tes(type:tUnit(), star, {tcon,{tycon,"()", star}}),
  tes(type:tChar(), star, {tcon,{tycon,"Char", star}}),
  tes(type:tInt(), star, {tcon,{tycon,"Int", star}}),
  tes(type:tInteger(), star, {tcon,{tycon,"Integer", star}}),
  tes(type:tFloat(), star, {tcon,{tycon,"Float", star}}),
  tes(type:tDouble(), star, {tcon,{tycon,"Double", star}}).

test2_test() ->
  % List[T] のようなパラメータが１つある型
  tes(type:tList(), {kfun, star, star},
    {tcon,{tycon,"List()", {kfun, star, star}}}),

  % T=>F のようなパラメータが２つある型
  tes(type:tArrow(), {kfun, star, {kfun, star, star}},
    {tcon,{tycon,"(=>)", {kfun, star, {kfun, star, star}}}}),

  % カンマもT,Fみたいに２つのパラメータが必要
  tes(type:tTuple2(), {kfun, star, {kfun, star, star}},
    {tcon,{tycon,"(,)", {kfun, star, {kfun, star, star}}}}).

test3_test() ->
  % fn関数で２つの型をしていして関数の型を生成出来る
  Fn_int_int = type:fn(type:tInt(),type:tInt()),

  % TApが2つある。
  tes(Fn_int_int, star,
    {tap,{tap,{tcon,{tycon,"(=>)",
      {kfun, star, {kfun, star, star}}}},
      {tcon,{tycon,"Int", star}}}, {tcon,{tycon,"Int", star}}}),

  % １つの型を指定してリスト型を生成できる
  List_int = type:list(type:tInt()),

  % TApが1つある。
  tes(List_int, star,
    {tap,{tcon,{tycon,"List()", {kfun, star, star}}},
      {tcon,{tycon,"Int", star}}}),

  % tStringはCharのリスト型だ。
  % TApが1つある。
  tes(type:tString(),
    star,
    {tap,{tcon,{tycon,"List()", {kfun, star, star}}},
      {tcon,{tycon,"Char", star}}}).

test4_test() ->
  % ペアは2つの型をもつのでTApが２つあると。
  Pair_int_char = type:pair(type:tInt(),type:tChar()),
  tes(Pair_int_char, star,
    {tap,{tap,{tcon,{tycon,"(,)", {kfun, star, {kfun, star, star}}}},
      {tcon,{tycon,"Int", star}}}, {tcon,{tycon,"Char", star}}}),

  Pair_int = {tap,{tcon,{tycon,"(,)", {kfun, star, {kfun, star, star}}}},
    {tcon,{tycon,"Int", star}}},

  tes(Pair_int, {kfun, star, star},
    {tap,{tcon,{tycon,"(,)", {kfun, star, {kfun, star, star}}}},
      {tcon,{tycon,"Int", star}}}).

