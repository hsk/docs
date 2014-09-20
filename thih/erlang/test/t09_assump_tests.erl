-module(t09_assump_tests).
-include_lib("eunit/include/eunit.hrl").

assump_test() ->
  T = {tvar,{tyvar,"a", star}},
  Assump = {assump,"ABC", {forall,[], {qual,[], T}}},
  Assump =
    {assump,"ABC", {forall,[], {qual,[], {tvar,{tyvar,"a", star}}}}}.

assumpApply_test() ->
  T = {tvar,{tyvar,"a", star}},
  Assump = {assump,"ABC", {forall,[], {qual,[], T}}},
  Subst = [{{tyvar,"a", star}, type:tInt()}],
  Assump2 = assump:assumpApply(Subst, Assump),
  Assump2 =
    {assump,"ABC", {forall,[], {qual,[], {tcon,{tycon,"Int", star}}}}}.

assumpTv_test() ->
  T = {tvar,{tyvar,"a", star}},
  Assump = {assump,"ABC", {forall,[], {qual,[], T}}},
  Tvs = assump:assumpTv(Assump),
  Tvs =
    [{tyvar,"a", star}].

assumpsApply_test() ->
  T = {tvar,{tyvar,"a", star}},
  Assump = {assump,"ABC", {forall,[], {qual,[], T}}},
  Subst = [{{tyvar,"a", star}, type:tInt()}],
  Assumps = assump:assumpsApply(Subst,[Assump]),
  Assumps =
    [
      {assump,"ABC",
        {forall,[], {qual,[], {tcon,{tycon,"Int", star}}}}}].

assumpsTv_test() ->
  T = {tvar,{tyvar,"a", star}},
  Assump = {assump,"ABC", {forall,[], {qual,[], T}}},
  Tvs = assump:assumpsTv([Assump]),
  Tvs =
    [{tyvar,"a", star}].

find_test() ->
  T = {tvar,{tyvar,"a", star}},
  Assump = {assump,"ABC", {forall,[], {qual,[], T}}},
  Assump2 = {assump,"ABC2", {forall,[], {qual,[], type:tInt()}}},
  Sc = assump:find("ABC",[Assump,Assump2]),
  Sc =
    {forall,[], {qual,[], {tvar,{tyvar,"a", star}}}}.
