-module(t11_1_lit_tests).
-include_lib("eunit/include/eunit.hrl").

tiLit_test() ->
  timonad:runTI(fun(Ti)->
    Lit = lit:litInt(123),
    {Preds, Ty} = lit:tiLit(Ti,Lit),

    Preds = [{isin,"Num", {tvar,{tyvar,"v0", star}}}],

    Ty = {tvar,{tyvar, "v0", star}},

    Subst = timonad:getSubst(Ti),
    Ty2 = subst:typeApply(Subst,Ty),
    Ty2 = Ty,

    Subst = []
  end).
