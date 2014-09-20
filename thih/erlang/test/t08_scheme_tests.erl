-module(t08_scheme_tests).
-include_lib("eunit/include/eunit.hrl").

scheme_test() ->

    Ty = {tvar,{tyvar,"a", star}},
    Pred = {isin,"Num", Ty},
    Sc = {forall,[], {qual,[Pred], Ty}},

    Sc =
      {forall,[],
        {qual,
          [{isin,"Num", {tvar,{tyvar,"a", star}}}],
          {tvar,{tyvar,"a", star}}}}.

schemeApply_test() ->
    Ty = {tvar,{tyvar,"a", star}},
    Pred = {isin,"Num", Ty},
    Sc = {forall,[], {qual,[Pred], Ty}},
    Subst = [{{tyvar,"a", star}, type:tInt()}],
    Sc1 = scheme:schemeApply(Subst, Sc),
    Sc1 =
      {forall,[],
        {qual,
          [{isin,"Num", {tcon,{tycon, "Int", star}}}],
          {tcon,{tycon, "Int", star}}}}.

schemeTv_test() ->
    Ty = {tvar,{tyvar,"a", star}},
    Pred = {isin,"Num", Ty},
    Sc = {forall,[], {qual,[Pred], Ty}},
    Tvs = scheme:schemeTv(Sc),
    Tvs =
      [{tyvar,"a", star}].

quantify_test() ->
    Tyvar = {tyvar,"a", star},
    Ty = {tvar,{tyvar,"a", star}},
    Pred = {isin,"Num", Ty},
    Qual = {qual,[Pred], type:fn(Ty, type:tInt())},
    Sc = scheme:quantify([Tyvar], Qual),
    io:format("Sc = ~p",[Sc]),
    Sc =
      {forall,[star],
        {qual,[{isin,"Num", {tgen,0}}],
          {tap,
            {tap,
              {tcon,{tycon, "(=>)", {kfun, star, {kfun, star, star}}}},
              {tgen,0}},
            {tcon,{tycon, "Int", star}}}}}.

toScheme_test() ->
    Ty = {tvar,{tyvar,"a", star}},
    Sc = scheme:toScheme(Ty),
    Sc =
      {forall,[], {qual,[], {tvar,{tyvar,"a", star}}}}.
