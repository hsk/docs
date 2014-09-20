-module(t06_unify_tests).
-include_lib("eunit/include/eunit.hrl").



t1() -> {tvar,{tyvar,"a", star}}.
t2() -> {tvar,{tyvar,"b", star}}.
tv1() -> {tyvar,"a", star}.
t3() -> type:tInt().

mgu_test() ->
  Subst = unify:mgu(t1(), t2()),

  [{{tyvar,"a", star}, {tvar,{tyvar,"b", star}}}]
  = Subst,
  
  Subst2 = unify:mgu(t1(), t3()),

  Subst2 = [{{tyvar,"a", star}, type:tInt()}].

varBind_test() ->
  Subst = unify:varBind(tv1(), t1()),
  [] = Subst,

  Subst2 = unify:varBind(tv1(), t3()),
  Subst2 = [{{tyvar,"a", star}, type:tInt()}].

match_test() ->
  Subst = unify:match(t1(), t2()),
  Subst = [{{tyvar,"a", star}, {tvar,{tyvar,"b", star}}}],

  Subst2 = unify:match(t1(), t3()),
  Subst2 = [{{tyvar,"a", star}, type:tInt()}].
