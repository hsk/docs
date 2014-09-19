-module(timain).
-export([

]).
% // 11.3 Expressions
% // 11.4 Alternatives
% // 11.5 From Types to Type Schemes
% // 11.6 Binding Groups

%   import Kind._
%   import Type._
%   import Pred._
%   import Subst._
%   import TIMonad._
%   import Infer._
%   import Lit._
%   import Pat._
%   import Scheme._
%   //import Assump._
 
%   type Ambiguity = (Tyvar, List[Pred])

ambiguities(Tyvars, Preds) ->
  Tyvars_ = pre:diff(pred:predsTv(Preds), Tyvars),
  lists:map(
    fun(V) ->
      {V, lists:filter(fun(P)-> lists:member(V, predTv(P)) end, Preds) }
    end,
    Tyvars_).

numClasses() ->
  ["Num", "Integral", "Floating", "Fractional", "Real", "RealFloat",
  "RealFrac"].

stdClasses() -> lists:append([
  "Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix",
  "Functor", "Monad", "MonadPlus"], numClasses()).

candidates(ClassEnv, Ambiguity) ->
  {V, Qs} = Ambiguity,
  Is = lists:map(fun({isin,I,_}) -> I end, Qs),
  Ts = lists:map(fun({isin,_,T}) -> T end, Qs),
  case lists:forall(fun(T)-> T == type:tvar(V) end, Ts) and
    lists:any(fun(I)-> lists:member(I, numClasses) end, Is) and
    lists:forall(fun(I)-> lists:member(I, stdClasses) end, Is) of

    true ->
      {classEnv, _, Defaults} = ClassEnv,
      lists:filter(
        fun(T_)->
          lists:forall(
            fun(I)-> entail(ClassEnv,[],I)end,
            lists:map(fun(I)->pred:isin(I, T_)end, Is)
          )
        end,
      Defaults);
    false -> []
  end.


withDefaults(F, ClassEnv, Tyvars, Preds) ->
  Vps = ambiguities(Tyvars, Preds),
  Tss = lists:map(fun(Vp)->candidates(ClassEnv,Vp)end, Vps),
  case (Tss.exists(fun pre:isEmpty/1)) of
    true -> throw("cannot resolve ambiguity");
    false -> F(Vps, lists:map(fun lists:hd/1, Tss)
  end.

defaultedPreds(ClassEnv, Tyvars, Preds) ->
  withDefaults(
    fun(Vps,Ts)->
      lists:map(
        fun(Vp)->lists:concat(element(2,Vp))end,
        Vps
      )
    end,
    ClassEnv, Tyvars, Preds
  ).

defaultSubst(ClassEnv, Tyvars Preds) ->
  withDefaults(
    fun(Vps,Ts) ->
      vps.map { _._1 }.zip(Ts)
    end,
    ClassEnv, Tyvars, Preds
  ).

split(ClassEnv, Tyvars, Tyvars2, Preds) ->
  Ps_ = reduce(ClassEnv, Preds),
  {Ds, Rs} =
    Ps_.partition(
      fun(P)->
        lists:forall(fun(I)-> lists:member(I,Tyvars) end, predTv(P))
      end
    ),
  Rs_ = defaultedPreds(ClassEnv, lists:append(Tyvars, Tyvars2), Rs),
  {Ds, pre:diff(Rs, Rs_)}.


%   sealed trait Expr
var(Id)->{var,Id}.
lit(Lit)->{lit,Lit}.
const(Assump)->{const,Assump}.
ap(E1,E2)->{ap,E1,E2}.
let_(BindGroup,E) -> {let_,BindGroup, E}.
% lam(Alt) -> {lam, Alt}.
% if_(E1,E2,E3) -> {if_,E1,E2,E3}.
% case_(E,PatEs) -> {case_,E,PatEs}.

%   type Alt = (List[Pat], Expr)
%   type Expl = (Id.Id, Scheme, List[Alt])
%   type Impl = (Id.Id, List[Alt])
%   type BindGroup = (List[Expl], List[List[Impl]])

restricted(Impls) ->
  lists:any(
    fun(_, Alts) ->
      lists:any(
        fun(Alt)->
          pre:isEmpty(element(1,Alt))
        end,
        Alts
      )
    end,
    Impls
  ).

tiSeq(F, Ti, ClassEnv, Assumps, BGs) ->
  case BGs of
    [] -> {[], []};
    [Bs | Bss] ->
      {Ps, Assumps2} = f(Ti, ClassEnv, Assumps, Bs),
      {Qs, Assumps_} =
        tiSeq(F, Ti, ClassEnv, lists:append(Assumps2, Assumps), Bss),
      {lists:append(Ps, Qs), lists:append(Assumps_, Assumps2)}
  end.

tiExpr(Ti, ClassEnv, Assumps, Expr) ->
  case Expr of
    {var,I} ->
      Sc = assump:find(I, Assumps),
      {qual, Ps, T} = freshInst(Ti, Sc),
      {Ps, T};
    {const,{assump, _, Sc}} ->
      {qual, Ps, T} = freshInst(Ti, Sc),
      {Ps, T};
    {lit, L} -> tiLit(Ti, L);
    {ap, E, F} ->
      {Ps, Te} = tiExpr(Ti, ClassEnv, Assumps, E),
      {Qs, Tf} = tiExpr(Ti, ClassEnv, Assumps, F),
      T = type:newTVar(Ti, kind:star()),
      unify(Ti, type:fn(Tf, T), Te),
      {lists:append(Ps, Qs), T}
    {let_,Bg, E} ->
      {Ps, As2} = tiBindGroup(Ti, ClassEnv, Assumps, Bg),
      {Qs, T} = tiExpr(Ti, ClassEnv, lists:append(As2, Assumps), E),
      {lists:append(Ps, Qs), T}

    % case Lam(alt) => tiAlt(ti, ClassEnv, as_)alt
    % case If(e, e1, e2) =>
    %   def (ps,t) = tiExpr(ti, ClassEnv, as_)e in
    %   unify(ti)t tBool;
    %   def (ps1,t1) = tiExpr(ti, ClassEnv, as_)e1 in
    %   def (ps2,t2) = tiExpr(ti, ClassEnv, as_)e2 in
    %   unify(ti)t1 t2;
    %   (ps @ ps1 @ ps2, t1)
    % case Case(e, branches) =>
    % def (ps, t) = tiExpr(ti, ClassEnv, as_)e in
    % def v = newTVar Star in
    % def tiBr (pat, f) =
    %   def (ps, _as',t') = tiPat pat in
    %   unify t t';
    %   def (qs, t'') = tiExpr (ClassEnv, _as' @ _as) f in
    %   unify v t'';
    %   (ps @ qs)
    % in
    % def pss = mapM tiBr branches in
    % (ps @ concat pss, v)
    
  end.

tiAlt(Ti, ClassEnv: ClassEnv, Assumps, Alt) ->
  {Pats, E} = Alt,
  {Ps, As1, Ts} = tiPats(Ti, Pats),
  {Qs, T} = tiExpr(Ti, ClassEnv, lists:append(As1, Assumps), E),
  {
    lists:append(Ps, Qs),
    lists:foldr(
      fun(A, B)-> type:fn(A, B)end,
      T,
      Ts
    )
  }.

tiAlts(Ti, ce: ClassEnv, as_ : Assumps, Alts, t: Type) ->
  {Ps, Ts} = lists:map(
    fun(Alt)->tiAlt(Ti, ClassEnv, Assumps,Alt) end,
    Alts).unzip,
  Ts.foreach { unify(Ti, T) },
  lists:concat(Ps).

tiExpl(Ti, ClassEnv, Assumps, Expl) ->
 
  {i, sc, alts} = Expl,
  Qual(qs, t) = freshInst(ti, sc),
  ps = tiAlts(ti, ClassEnv, Assumps, alts, t),
  s = getSubst(ti),
  qs2 = predsApply(s, qs),
  t2 = typeApply(s, t),
  fs = assump:assumpsTv(assump:assumpsApply(s, Assumps)),
  gs = Pre.diff(typeTv(t2), fs),
  sc2 = quantify(gs, Qual(qs2, t2)),
  Ps_ = predsApply(s, ps).filter { p => not entail(ClassEnv, qs2, p) },
  {Ds, Rs} = split(ClassEnv, fs, gs, ps_),
  if (Sc /= Sc2) throw("signature too general") end,
  case (not pre:isEmpty(Rs)) of
    true -> throw("context too weak");
    _ -> Ds
  end.
 
tiImpls(Ti, ClassEnv, Assumps, Impls) ->
  Ts = lists:map(fun(I) -> type:newTVar(ti, Star, I) end, Impls),
  {Is, Altss} = lists:unzip(Impls),
  Scs = lists:map(fun scheme:toScheme/1, Ts),
  As1 = lists:append(
    lists:map(
      fun({I, Sc}) -> assump:assump(I, Sc)end, 
      lists:zip(Is,Scs)
    ),
    Assumps
  ),
  Pss = lists:map(
    fun({A, B}) -> tiAlts(Ti, ClassEnv, As1, A, B) end,
    lists:zip(Altss, Ts)
  ),
  S = getSubst(Ti),
  Ps_ = lists:map(fun(I)->predApply(S,I)end ,lists:concat(Pss)),
  Ts2 = lists:map(fun(I)->typeApply(S,I) end, Ts),
  Fs = assump:assumpsTv(assump:assumpsApply(S, Assumps)),
  Vss = lists:map(fun typeTv/1, Ts2),
  Vss2 = pre:fold_left1(
    fun(A, B) -> pre:union(A, B) end,
    Vss
  ),
  Gs = pre:diff(Vss2, Fs),
  {Ds, Rs} =
    split(
      ClassEnv,
      Fs,
      pre:fold_left1(fun(A, B) -> pre:intersect(A, B) end, Vss),
      Ps_
    ),

  case restricted(Impls) of
    true ->
      gs2 = Pre.diff(gs, predsTv(rs)),
      scs2 = ts2.map { t => quantify(gs2, Qual(List(), t)) },
      {
        ds ::: rs,
        is.zip(scs2).map { case (i, sc) => Assump.Assump(i, sc) }
      };
    false ->
      scs1 = ts2.map { t => quantify(gs, Qual(rs, t)) },
      {
        ds,
        is.zip(scs1).map { case (i, sc) => Assump.Assump(i, sc) }
      }
  end.
 
tiBindGroup(Ti, ClassEnv, Assumps, BindGroup) ->
    {Es, Iss} = BindGroup,
    As1 = Es.map { case (V, Sc, _) => assump:assump(V, Sc) },
    {Ps, As2} = tiSeq(fun tiImpls/2, Ti, ClassEnv, As1 ::: Assumps, Iss),
    Qss = Es.map { tiExpl(Ti, ClassEnv, As2 ::: As1 ::: Assumps) },
    {Ps ::: (Qss.flatten), As2 ::: As1}

%   type Program = List[BindGroup]
 
tiProgram(ClassEnv, Assumps, Program) ->
  runTI(
    fun(Ti)->
      {Ps, As2} = tiSeq(fun tiBindGroup/5, Ti, ClassEnv, Assumps, Program),
      S = getSubst(Ti),
      Rs = reduce(ClassEnv, predsApply(S, Ps)),
      S2 = defaultSubst(ClassEnv, [], Rs),
      assump:assumpsApply(atat(S2, S), As2)
    end
  ).
