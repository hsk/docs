% 10 A Type Inference Monad
-module(timonad).
-export([
  ti/2,
  runTI/1,
  getSubst/1,
  extSubst/2,
]).

% import Kind._
% import Type._
% import Subst._
% import Pred._
% import Scheme._

ti(Subst,N) -> {ti,Subst,N}.

runTI(F) ->
  F(ti(subst:nullSubst(), 0)).

getSubst({ti, Subst, N}) -> Subst.

extSubst(Ti, Subst) ->
  Ti.s = atat(Subst, Ti.s)

unify(Ti, Type1, Type2) ->
  Subst = getSubst(Ti),
  U = unify:mgu(typeApply(Subst,Type1),typeApply(Subst,Type2)),
  extSubst(Ti, U).

newTVar(Ti, Kind) ->
  V = type:tyvar(id:enumId(Ti.n), Kind),
  Ti.n += 1,
  type:tvar(V).

typeInst(Types, {tap, L, R}) ->
  type:tap(typeInst(Types, L), typeInst(Types, R));
typeInst(Types, {tgen, N}) -> Types(N);
typeInst(Types, Type) -> Type.

listInst(Inst, Types, Xs) ->
  lists:map(Inst(Types), Xs).

predInst(Types, Pred) ->
  {isin, C, T} = Pred,
  pred:isin(C, typeInst(Types, T)).

qualTypeInst(ts: Types, q: Qual) ->
  {qual, Ps, T} = Qual,
  pred:qual(listInst(fun predInst/2, Types, Ps), typeInst(Types,T)).

freshInst(Ti, Scheme) ->
  {forall, Ks, Qt} = Scheme,
  Ts = lists:map(fun(K)->newTVar(Ti, K)end, Ks),
  qualTypeInst(Types, Qt).
