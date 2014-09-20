-module(t10_timonad_tests).
-include_lib("eunit/include/eunit.hrl").

ti_test() ->
  Subst = [{{tyvar,"a", star}, type:tInt()}],
  Ti = timonad:ti(Subst, 1),
  N = timonad:get(Ti, count),
  N = 1,
  Subst2 = timonad:get(Ti, subst),
  Subst2 = [{{tyvar,"a", star}, {tcon,{tycon,"Int", star}}}].

runTI_test() ->
  N = timonad:runTI(fun(Ti)->
    N = timonad:get(Ti, count) + 1,
    timonad:set(Ti, count, N),
    N
  end),
  N = 1.

getSubst_test() ->
  timonad:runTI(fun(Ti)->
    Subst = timonad:getSubst(Ti),
    Subst = []
  end).

extSubst_test() ->
  timonad:runTI(fun(Ti)->
    Subst = [{{tyvar,"a", star}, type:tInt()}],
    timonad:extSubst(Ti, Subst),
    SubsT2 = timonad:getSubst(Ti),
    SubsT2 = [{{tyvar,"a", star}, {tcon,{tycon,"Int", star}}}]
  end).

unify_test() ->
  timonad:runTI(fun(Ti)->
    T1 = {tvar,{tyvar,"a", star}},
    timonad:unify(Ti, T1, type:tInt()),
    T1 = {tvar,{tyvar,"a", star}}
  end).

newTVar_test() ->
  timonad:runTI(fun(Ti)->
    T1 = timonad:newTVar(Ti, star),
    T1 = {tvar,{tyvar,"v0", star}},
    timonad:unify(Ti, T1, type:tInt()),
    T1 = {tvar,{tyvar,"v0", star}},
    T2 = subst:typeApply(timonad:getSubst(Ti), T1),
    T2 = type:tInt()
  end).

freshInst_test() ->
  timonad:runTI(fun(Ti)->
    Ty = {tvar,{tyvar,"a", star}},
    Sc = scheme:toScheme(Ty),
    Tq = timonad:freshInst(Ti, Sc),
    Sc = {forall,[], {qual,[], {tvar,{tyvar,"a", star}}}},
    Tq = {qual,[], {tvar,{tyvar,"a", star}}}
  end).
