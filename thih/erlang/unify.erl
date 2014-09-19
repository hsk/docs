% 6 Unification and Matching
-module(unify).
-export([mgu/2,varBind/2,match/2]).

mgu({tap,L,R}, {tap,L_,R_}) ->
  S1 = mgu(L, L_),
  S2 = mgu(subst:typeApply(S1, R), subst:typeApply(S1, R_)),
  subst:atat(S2, S1);
mgu({tvar,U}, Type) -> varBind(U, Type);
mgu(Type, {tvar,U}) -> varBind(U, Type);
mgu({tcon,Tc1}, {tcon,Tc2}) when Tc1 == Tc2 -> subst:nullSubst();
mgu(_, _) -> throw("types do not unify").

varBind(Tyvar, Type) ->
  Subst = if
    Type == {tvar,Tyvar} -> {ok,subst:nullSubst()};
    true -> {ng}
  end,

  Occurs = lists:member(Tyvar, type:typeTv(Type)),
  if Occurs -> throw("occurs check fails") end,
  KindCheck = (type:tyvarKind(Tyvar) /= type:typeKind(Type)),
  if KindCheck -> throw("kinds do not match") end,
  case Subst of
  	{ok,V} -> V;
  	{ng} -> type:tyvar_append(Tyvar, Type)
  end
  .

match({tap, L, R}, {tap, L_, R_}) ->
  Sl = match(L,L_),
  Sr = match(R,R_),
  subst:merge(Sl, Sr);
match({tvar,U}, T) ->
  Ck = (type:tyvarKind(U) == type:typeKind(T)),
  if
  	Ck -> type:tyvar_append(U,T);
  	true -> throw("types do not match")
  end;
match({tcon,Tc1}, {tcon,Tc2}) when(Tc1 == Tc2) -> subst:nullSubst();
match(_,_) -> throw("types do not match").
