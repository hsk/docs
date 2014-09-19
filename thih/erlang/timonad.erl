% 10 A Type Inference Monad
-module(timonad).
-export([
  ti/2,
  runTI/1,
  getSubst/1,
  extSubst/2,
  unify/3,
  newTVar/2,
  typeInst/2,
  listInst/3,
  predInst/2,
  qualTypeInst/2,
  freshInst/2
]).

% import Kind._
% import Type._
% import Subst._
% import Pred._
% import Scheme._

set(Ti,K,V) -> ets:insert(Ti, {K, V}).
get(Ti,K) -> ets:lookup(Ti, K).

ti(Subst,N) ->
  Ti = ets:new(ti, [ set ]),
  set(Ti, subst, Subst),
  set(Ti, count, N),
  Ti.

runTI(F) ->
  F(ti(subst:nullSubst(), 0)).

getSubst(Ti) -> get(Ti,subst).

extSubst(Ti, Subst) ->
  set(Ti, subst, subst:'@@'(Subst, get(Ti,subst))).

unify(Ti, Type1, Type2) ->
  Subst = getSubst(Ti),
  Subst2 = unify:mgu(subst:typeApply(Subst,Type1),subst:typeApply(Subst,Type2)),
  extSubst(Ti, Subst2).

newTVar(Ti, Kind) ->
  N = get(Ti,count),
  set(Ti,count,N+1),
  V = type:tyvar(id:enumId(N), Kind),
  type:tvar(V).

typeInst(Types, {tap, L, R}) ->
  type:tap(typeInst(Types, L), typeInst(Types, R));
typeInst(Types, {tgen, N}) -> Types(N);
typeInst(_, Type) -> Type.

listInst(Inst, Types, Xs) ->
  lists:map(Inst(Types), Xs).

predInst(Types, Pred) ->
  {isin, C, T} = Pred,
  pred:isin(C, typeInst(Types, T)).

qualTypeInst(Types, Qual) ->
  {qual, Ps, T} = Qual,
  pred:qual(listInst(fun predInst/2, Types, Ps), typeInst(Types,T)).

freshInst(Ti, Scheme) ->
  {forall, Ks, Qt} = Scheme,
  Types = lists:map(fun(K)->newTVar(Ti, K)end, Ks),
  qualTypeInst(Types, Qt).
