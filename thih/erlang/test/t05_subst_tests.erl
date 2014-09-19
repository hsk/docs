-module(t05_subst_tests).
-include_lib("eunit/include/eunit.hrl").


nullSubst_test() ->
  R = subst:nullSubst(),
  R = subst:nullSubst().

'+->_test'() ->
  1 = 1.
  % substは+->演算子で作れる
%  Subst = {tyvar, "a", star} +-> type:tInt(),
%
%  Subst = [
%    (Tyvar("a", star), TCon(Tycon("Int", star))))
%
%  Subst1 = Tyvar("b", star) +-> tChar
%
%  % substはリストなので ::: で結ß合出来る
%  Subst2 = subst ::: subst1
%
%  subst2 = [
%    (Tyvar("a", star), TCon(Tycon("Int", star))),
%    (Tyvar("b", star), TCon(Tycon("Char", star)))],
%
%  % typeApplyはsubstを元に型変数がsubstにあれば置き換える。
%
%  val tva = TVar(Tyvar("a", star))
%  val tvb = TVar(Tyvar("b", star))
%  typeApply(subst, tva) = TCon(Tycon("Int", star))
%  typeApply(subst, tvb) = TVar(Tyvar("b", star))
%
%  % TApの中身も置き換わる
%  val tap = TAp(tva, tvb)
%  typeApply(subst, tap) =
%    TAp(TCon(Tycon("Int", star)), TVar(Tyvar("b", star)))
%
%  val tap2 = TAp(tva, tva)
%  typeApply(subst, tap2) =
%    TAp(TCon(Tycon("Int", star)), TCon(Tycon("Int", star)))
%
%  % typeTvでは内部で使っている型変数のリストを返す
%  typeTv(tva) = [Tyvar("a", star))
%  typeTv(tvb) = [Tyvar("b", star))
%
%  % tapは2つの型を使っているのでaとbが返る
%  typeTv(tap) = [Tyvar("a", star), Tyvar("b", star))
%
%  % listApply
%  %def listApply[A,B](apply: Subst => A => B, s : Subst, xs:List[A]):List[B] = {
%  %  xs.map(apply(s))
%  %}
%
%  % listApplyは複数の型を受け取って、展開する
%  listApply { typeApply }(subst, [tva, tvb)) = [
%    TCon(Tycon("Int", star)),
%    TVar(Tyvar("b", star)))
%
%  % listTvはlist全体の内部で使っている型変数を求める
%  % listTv
%  %def listTv[A](tv: A => List[Tyvar], xs:List[A]) : List[Tyvar] = {
%  %  Pre.nub(xs.map(tv).flatten)
%  %}
%  listTv(typeTv, [tva, tap)) =
%    [
%      Tyvar("b", star),
%      Tyvar("a", star)]
%
%'@@_test'() ->
%
%  Subst = Tyvar("a", star) +-> type:tInt()
%  Subst1 = Tyvar("b", star) +-> TVar(Tyvar("a", star)) ::: Tyvar("a", star) +-> tChar
%  Subst2 = Tyvar("b", star) +-> TVar(Tyvar("a", star)) ::: Tyvar("a", star) +-> type:tInt()
%
%  % @@演算子で2つのsubstを結合出来る。最初のsubstをs2に実行して結合する
%  (subst @@ subst1) = [
%    (Tyvar("b", star), TCon(Tycon("Int", star))),
%    (Tyvar("a", star), TCon(Tycon("Char", star))),
%    (Tyvar("a", star), TCon(Tycon("Int", star))))
%
%  (subst @@ subst2) = [
%    (Tyvar("b", star), TCon(Tycon("Int", star))),
%    (Tyvar("a", star), TCon(Tycon("Int", star))),
%    (Tyvar("a", star), TCon(Tycon("Int", star))))
%  % merge
%  %def merge(s1:Subst, s2:Subst) : Subst = {
%  %  val agree = {
%  %    Pre.intersect(s1.map(_._1), s2.map(_._1)).forall{ v =>
%  %      typeApply(s1, TVar(v)) == typeApply(s2, TVar(v))
%  %    }
%  %  }
%  %  if(agree) s1 ::: s2
%  %  else throw new Exception("substitutions do not agree")
%  %}
%
%  try {
%    % @@と似ているのだけど、s1とs2でおかしいものがあったらエラーにする
%    merge(subst, subst1)
%    assert(false)
%  } catch {
%    case _: Throwable =>
%  }
%
%  merge(subst, subst2) = [
%    (Tyvar("a", star), TCon(Tycon("Int", star))),
%    (Tyvar("b", star), TVar(Tyvar("a", star))),
%    (Tyvar("a", star), TCon(Tycon("Int", star))))
%
