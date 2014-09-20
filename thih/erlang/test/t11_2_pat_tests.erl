-module(t11_2_pat_tests).
-include_lib("eunit/include/eunit.hrl").

tiPat_PVar_test() ->
  timonad:runTI(fun(Ti) ->
    Pat = {pvar, "a"},
    {Preds, Assumps, Ty} = pat:tiPat(Ti, Pat),

    Preds = [],
    Assumps =
      [
        {assump,"a",{forall,[],{qual,[],{tvar,{tyvar,"v0",star}}}}}],

    Ty = {tvar,{tyvar,"v0", star}}
  end).

tiPat_pwildcard_test() ->
  timonad:runTI(fun(Ti) ->
    Pat = pwildcard,
    {Preds, Assumps, Ty} = pat:tiPat(Ti, Pat),

    Preds = [],
    Assumps = [],
    Ty = {tvar,{tyvar,"v0", star}}
  end).

tiPat_PAs_test() ->
  timonad:runTI(fun(Ti) ->
    Pat = {pas, "a", pwildcard},
    {Preds, Assumps, Ty} = pat:tiPat(Ti, Pat),

    Preds = [],
    Assumps =
      [
        {assump,"a",{forall,[],{qual,[],{tvar,{tyvar,"v0",star}}}}}],

    Ty = {tvar,{tyvar,"v0", star}}
  end).

tiPat_PLit_test() ->
  timonad:runTI(fun(Ti) ->
    Pat = {plit,lit:litInt(123)},
    {Preds, Assumps, Ty} = pat:tiPat(Ti, Pat),

    Preds =
      [{isin,"Num",{tvar,{tyvar,"v0",star}}}],
    Assumps = [],
    Ty = {tvar,{tyvar,"v0", star}}
  end).

tiPat_PNpk_test() ->
  timonad:runTI(fun(Ti) ->
    Pat = {pnpk,"a",10},
    {Preds, Assumps, Ty} = pat:tiPat(Ti, Pat),

    Preds =
      [{isin,"Integral",{tvar,{tyvar,"v0",star}}}],
    Assumps =
      [
        {assump,"a",{forall,[],{qual,[],{tvar,{tyvar,"v0",star}}}}}],

    Ty = {tvar,{tyvar,"v0", star}}
  end).

tiPat_PCon_test() ->
  timonad:runTI(fun(Ti) ->
    T = {tvar,{tyvar,"a", star}},
    Assump = {assump,"ABC", {forall,[], {qual,[], T}}},

    Pat = {pcon,Assump,[]},
    {Preds, Assumps, Ty} = pat:tiPat(Ti, Pat),

    Preds = [],
    Assumps = [],
    Ty = {tvar,{tyvar,"v0", star}}
  end).
