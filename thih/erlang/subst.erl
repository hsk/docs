% 5 Substitutions
-module(subst).
-export([
  nullSubst/0,
  typeApply/2,
  typeTv/1,
  listApply/3,
  listTv/2,
  '@@'/2,
  merge/2
]).

%   type Subst = List[(Tyvar, Type_)]

nullSubst() -> [].

% 型変数を展開する
typeApply(Subst, {tvar,U}) ->
  case lists:find(fun(K,_) -> K == U end, Subst) of
    {ok, {_,Type}} -> Type;
    _ -> {tvar, U}
  end;
typeApply(Subst, {tap,L,R}) ->
  type:tap(typeApply(Subst, L), typeApply(Subst,R));
typeApply(_, T) -> T.

typeTv({tvar,U}) -> [U];
typeTv({tap,L,R}) -> pre:union(typeTv(L),typeTv(R));
typeTv(_) -> [].

listApply(Apply, Subst, Xs) ->
  lists:map(fun (X) -> Apply(Subst, X) end, Xs).

listTv(F, Xs) ->
  pre:nub(lists:flatten(lists:map(F, Xs))).

'@@'(Subst1,Subst2) ->
  lists:append(
    lists:map(fun(U,T) -> {U, typeApply(Subst1,T)} end,Subst2),
    Subst1).

merge(Subst1, Subst2) ->
  Agree =
   pre:intersect(
    lists:map(fun(V)->element(1,V) end, Subst1),
    lists:forall(
      fun(V) ->
        typeApply(Subst1,type:tvar(V)) ==
        typeApply(Subst2,type:tvar(V))
      end,
      lists:map(fun(V)->element(1,V) end, Subst2)
    )
  ),
  if
    Agree -> lists:append(Subst1,Subst2);
    true -> throw("substitutions do not agree") 
  end.
