% 6 Unification and Matching
-module(unify).
-export([mgu/2,varBind/2,match/2]).

mgu({tap,L,R}, {tap,L_,R_}) ->
  S1 = mgu(L, L_),
  S2 = mgu(subst:typeApply(S1, R), subst:typeApply(S1, R_)),
  subst:'@@'(S2, S1);
mgu({tvar,U}, Type) -> varBind(U, Type);
mgu(Type, {tvar,U}) -> varBind(U, Type);
mgu({tcon,Tc1}, {tcon,Tc2}) when Tc1 == Tc2 -> subst:nullSubst();
mgu(_, _) -> throw("types do not unify").

varBind(Tyvar, Type) ->
  case Type == {tvar,Tyvar} of
    true -> subst:nullSubst();
    false ->
      case lists:member(Tyvar, subst:typeTv(Type)) of
        true -> throw("occurs check fails");
        false -> o
      end,
      case type:tyvarKind(Tyvar) /= type:typeKind(Type) of
        true -> throw("kinds do not match");
        false -> type:tyvar_append(Tyvar, Type)
      end
  end.

match({tap, L, R}, {tap, L_, R_}) ->
  subst:merge(match(L,L_), match(R,R_));
match({tvar,U}, T) ->
  case type:tyvarKind(U) == type:typeKind(T) of
  	true -> type:tyvar_append(U,T);
  	false -> throw("types do not match")
  end;
match({tcon,Tc1}, {tcon,Tc2}) when(Tc1 == Tc2) -> subst:nullSubst();
match(_,_) -> throw("types do not match").
