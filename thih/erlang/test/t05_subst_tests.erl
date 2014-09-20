-module(t05_subst_tests).
-include_lib("eunit/include/eunit.hrl").


nullSubst_test() ->
  R = subst:nullSubst(),
  R = subst:nullSubst().

'+->_test'() ->
  % substは+->演算子で作れる
  Subst = type:'+->'({tyvar, "a", star}, type:tInt()),

  [
    {{tyvar, "a", star}, {tcon, {tycon, "Int", star}}}
  ] = Subst,

  Subst1 = type:'+->'({tyvar, "b", star}, type:tChar()),

  % substはリストなので ::: で結ß合出来る
  Subst2 = lists:append(Subst, Subst1),
  Subst2 = [
    {{tyvar, "a", star}, {tcon, {tycon, "Int", star}}},
    {{tyvar, "b", star}, {tcon, {tycon, "Char", star}}}
  ],

  % subst:typeApplyはsubstを元に型変数がsubstにあれば置き換える。

  Tva = {tvar,{tyvar, "a", star}},
  Tvb = {tvar,{tyvar, "b", star}},
  {tcon,{tycon, "Int", star}} = subst:typeApply(Subst, Tva),
  {tvar,{tyvar, "b", star}} = subst:typeApply(Subst, Tvb),

  % TApの中身も置き換わる
  Tap = {tap, Tva, Tvb},
  {tap, {tcon, {tycon, "Int", star}}, {tvar,{tyvar, "b", star}}}
  = subst:typeApply(Subst, Tap),

  Tap2 = {tap, Tva, Tva},
  {tap, {tcon, {tycon, "Int", star}}, {tcon, {tycon, "Int", star}}}
  = subst:typeApply(Subst, Tap2),

  % subst:typeTvでは内部で使っている型変数のリストを返す
  [{tyvar, "a", star}] = subst:typeTv(Tva),
  [{tyvar, "b", star}] = subst:typeTv(Tvb),

  % tapは2つの型を使っているのでaとbが返る
  [{tyvar, "a", star}, {tyvar, "b", star}]
  = subst:typeTv(Tap),

  % listApply
  %def listApply[A,B](apply: Subst => A => B, s : Subst, xs:List[A]):List[B] = {
  %  xs.map(apply(s))
  %}

  % listApplyは複数の型を受け取って、展開する
  [
    {tcon, {tycon, "Int", star}},
    {tvar, {tyvar, "b", star}}
  ]
  = subst:listApply(fun subst:typeApply/2, Subst, [Tva, Tvb]),

  % listTvはlist全体の内部で使っている型変数を求める
  % listTv
  %def listTv[A](tv: A => List[Tyvar], xs:List[A]) : List[Tyvar] = {
  %  Pre.nub(xs.map(tv).flatten)
  %}
  [
    {tyvar, "b", star},
    {tyvar, "a", star}
  ]
  = subst:listTv(fun subst:typeTv/1, [Tva, Tap]).

'@@_test'() ->

  Subst = type:'+->'({tyvar, "a", star}, type:tInt()),
  Subst1 =
    lists:append(
      type:'+->'({tyvar, "b", star}, {tvar,{tyvar, "a", star}}),
      type:'+->'({tyvar, "a", star}, type:tChar())
    ),
  Subst2 =
    lists:append(
      type:'+->'({tyvar, "b", star}, {tvar,{tyvar, "a", star}}),
      type:'+->'({tyvar, "a", star}, type:tInt())
    ),
  % @@演算子で2つのsubstを結合出来る。最初のsubstをs2に実行して結合する

  [
    {{tyvar, "b", star}, {tcon, {tycon, "Int", star}}},
    {{tyvar, "a", star}, {tcon, {tycon, "Char", star}}},
    {{tyvar, "a", star}, {tcon, {tycon, "Int", star}}}
  ]
  = subst:'@@'(Subst,Subst1),

  [
    {{tyvar, "b", star}, {tcon, {tycon, "Int", star}}},
    {{tyvar, "a", star}, {tcon, {tycon, "Int", star}}},
    {{tyvar, "a", star}, {tcon, {tycon, "Int", star}}}
  ] = subst:'@@'(Subst,Subst2),
  % merge
  %def merge(s1:Subst, s2:Subst) : Subst = {
  %  val agree = {
  %    Pre.intersect(s1.map(_._1), s2.map(_._1)).forall{ v =>
  %      subst:typeApply(s1, {tvar,v)) == subst:typeApply(s2, {tvar,v))
  %    }
  %  }
  %  if(agree) s1 ::: s2
  %  else throw new Exception("substitutions do not agree")
  %}

    % @@と似ているのだけど、s1とs2でおかしいものがあったらエラーにする
  ?assert(try subst:merge(Subst, Subst1) of
    _ -> false
  catch
    _ -> true
  end),
  [
    {{tyvar, "a", star}, {tcon, {tycon, "Int", star}}},
    {{tyvar, "b", star}, {tvar, {tyvar, "a", star}}},
    {{tyvar, "a", star}, {tcon, {tycon, "Int", star}}}
  ] = subst:merge(Subst, Subst2).
