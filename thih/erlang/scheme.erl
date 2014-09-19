% 8 Type Schemes
-module(scheme).
-export([
  forall/2,
  schemeApply/2,
  schemeTv/1,
  quantify/2,
  toScheme/1
]).

%   import Kind._
%   import Type._
%   import Pred._

forall(Kinds,QualTypes) -> {forall,Kinds,QualTypes}.

schemeApply(Subst, Scheme) ->
  {forall, Ks, Qt} = Scheme,
  forall(Ks, pred:qualTypeApply(Subst, Qt)).

schemeTv(Scheme) ->
  {forall, _, Qt} = Scheme,
  pred:qualTypeTv(Qt).

quantify(Tyvars,QualType) ->
  Tyvars_ = lists:filter(
    fun(Tyvar) -> lists:member(Tyvar, Tyvars) end,
    pred:qualTypeTv(QualType)),
  Ks = lists:map(fun type:tyvarKind/1,Tyvars_),
  {S,_} = lists:foldl(
    fun ({Gens,Count}, Tyvar) ->
      T = type:tgen(Count),
      {[{Tyvar, T}|Gens], Count + 1}
    end,
    {[],0},
    Tyvars_
  ),
  forall(Ks, pred:qualTypeApply(S, QualType)).

toScheme(Type) ->
  forall([], pred:qual([], Type)).
