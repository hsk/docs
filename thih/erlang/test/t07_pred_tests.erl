-module(t07_pred_tests).
-include_lib("eunit/include/eunit.hrl").

pred_test() ->
  Ty = {tvar,{tyvar,"a", star}},
  Pred = {isin, "Num", Ty},

  Pred =
    {isin, "Num", {tvar,{tyvar,"a", star}}}.

pred_list_test() ->
  Ty = {tvar,{tyvar,"a", star}},
  Preds = [{isin, "Num", Ty}, {isin, "B", Ty}],

  Preds =
    [
      {isin, "Num", {tvar,{tyvar,"a", star}}},
      {isin, "B", {tvar,{tyvar,"a", star}}}].

pred_and_qual_test() ->
  % (Num a) => a -> Int

  Ty = {tvar,{tyvar,"a", star}},
  Pred = {isin, "Num", Ty},
  Pred = {isin, "Num", {tvar,{tyvar,"a", star}}},

  % Qual
  Q = {qual,[Pred], type:fn(Ty, type:tInt())},

  Q = {qual,[{isin, "Num", {tvar,{tyvar,"a", star}}}],
    {tap,{tap,{tcon,{tycon,"(=>)", {kfun,star, {kfun,star, star}}}},
      {tvar,{tyvar,"a", star}}}, {tcon,{tycon,"Int", star}}}}.

predApply_test() ->
  Subst = [{{tyvar,"a", star}, type:tInt()}],
  Pred = {isin, "Num", {tvar,{tyvar,"a", star}}},
  Pred2 = pred:predApply(Subst, Pred),
  Pred2 = {isin, "Num", {tcon,{tycon,"Int", star}}}.

predTv_test() ->
  Pred = {isin, "Num", {tvar,{tyvar,"a", star}}},
  Tvs = pred:predTv(Pred),
  Tvs = [{tyvar,"a", star}].

predsApply_test() ->
  Subst = [{{tyvar,"a", star}, type:tInt()}],
  Preds = [{isin, "Num", {tvar,{tyvar,"a", star}}}],
  Preds2 = pred:predsApply(Subst, Preds),
  Preds2 = [{isin, "Num", {tcon,{tycon,"Int", star}}}].

predsTv_test() ->
  Preds = [{isin, "Num", {tvar,{tyvar,"a", star}}}],
  Tvs = pred:predsTv(Preds),
  Tvs = [{tyvar,"a", star}].

qualTypeApply_test() ->
  Subst = type:'+->'({tyvar,"a", star}, type:tInt()),
  Ty = {tvar,{tyvar,"a", star}},
  Pred = {isin, "Num", Ty},
  Qual = {qual,[Pred], type:fn(Ty, type:tInt())},
  Qual2 = pred:qualTypeApply(Subst, Qual),

  Qual = {qual,
    [
      {isin, "Num", {tvar,{tyvar,"a", star}}}],
    {tap,{tap,{tcon,{tycon,"(=>)", {kfun,star, {kfun,star, star}}}},
              {tvar,{tyvar,"a", star}}},
         {tcon,{tycon,"Int", star}}}},

  Qual2 = {qual,
    [
      {isin, "Num", {tcon,{tycon,"Int", star}}}],
    {tap,{tap,{tcon,{tycon,"(=>)", {kfun,star, {kfun,star, star}}}},
        {tcon,{tycon,"Int", star}}}, {tcon,{tycon,"Int", star}}}}.

qualTypeTv_test() ->
  Ty = {tvar,{tyvar,"a", star}},
  Pred = {isin, "Num", Ty},
  Qual = {qual,[Pred], type:fn(Ty, type:tInt())},
  Tvs = pred:qualTypeTv(Qual),
  Tvs = [{tyvar,"a", star}].

mguPred_test() ->
  Pred1 = {isin, "Num", {tvar,{tyvar,"a", star}}},
  Pred2 = {isin, "Num", type:tInt()},

  Subst = pred:mguPred(Pred1, Pred2),
  Subst =
    [{{tyvar,"a",star},{tcon,{tycon,"Int",star}}}],

  Subst2 = pred:mguPred(Pred1, Pred1),
  Subst2 = [].

matchPred_test() ->
  Pred1 = {isin, "Num", {tvar,{tyvar,"a", star}}},
  Pred2 = {isin, "Num", type:tInt()},

  Subst = pred:matchPred(Pred1, Pred2),
  Subst = [{{tyvar,"a", star}, type:tInt()}],

  Subst2 = pred:matchPred(Pred1, Pred1),
  Subst2 = [{{tyvar,"a", star}, {tvar,{tyvar,"a", star}}}].

inst_test() ->
  Inst = {qual,
    [
      {isin, "Ord", type:tUnit()},
      {isin, "Ord", type:tChar()}],
    {isin, "Ord", type:tChar()}},

  Inst = {qual,
    [
      {isin, "Ord", {tcon,{tycon,"()", star}}},
      {isin, "Ord", {tcon,{tycon,"Char", star}}}],
    {isin, "Ord", {tcon,{tycon,"Char", star}}}}.

'class_:=>_test'() ->
  {
    ["Eq"],
    [
      pred:':=>'([], {isin, "Ord", type:tUnit()}),
      pred:':=>'([], {isin, "Ord", type:tChar()}),
      pred:':=>'([], {isin, "Ord", type:tInt()}),
      pred:':=>'(
        [
          {isin, "Ord", {tvar,{tyvar,"a", star}}},
          {isin, "Ord", {tvar,{tyvar,"b", star}}}
        ],
        {isin, "Ord",
          type:pair({tvar,{tyvar,"a", star}}, {tvar,{tyvar,"b", star}})
        }
      )
    ]
  }.

% 7.2 Class Environments

modify_test() ->
  Ce = pred:modify(pred:initialEnv(), "ABC", {["A"],
    [ pred:':=>'([], {isin, "Ord", type:tUnit()}) ]}
  ),
  {classEnv,_,Defaults} = Ce,
  Defaults =
    [{tcon,{tycon,"Integer", star}}, {tcon,{tycon,"Double", star}}].

super_test() ->
  Ce = pred:modify(pred:initialEnv(), "ABC", {["A"],
    [ pred:':=>'([],{isin, "Ord", type:tUnit()})]}),
  S = pred:super(Ce, "ABC"),
  S = ["A"].

insts_test() ->
  Ce = pred:modify(pred:initialEnv(), "ABC", {["A"],
    [pred:':=>'([], {isin, "Ord", type:tUnit()})]}),
  S = pred:insts(Ce, "ABC"),
  S = [{qual,[], {isin, "Ord", {tcon,{tycon,"()", star}}}}].

defined_test() ->
  Ce = pred:modify(pred:initialEnv(), "ABC", {["A"],
    [pred:':=>'([],{isin, "Ord", type:tUnit()})]
  }),
  S = pred:defined(Ce, "ABC"),
  S = true.

addClass_test() ->
  Et = pred:addClass("Eq", []),
  Ce = Et(pred:initialEnv()),
  {classEnv,_,Defaults} = Ce,
  Defaults =
    [{tcon,{tycon,"Integer", star}}, {tcon,{tycon,"Double", star}}].

'<:>_test'() ->
  Et1 = pred:addClass("Eq", []),
  Et2 = pred:addClass("Eq2", []),
  Et3 = pred:'<:>'(Et1, Et2),
  {classEnv,_,Defaults} = Et3(pred:initialEnv()),
  Defaults =
    [{tcon,{tycon,"Integer", star}}, {tcon,{tycon,"Double", star}}],
  Et4 = pred:'<:>'(
    pred:addClass("Eq", []),
    pred:addClass("Eq2", [])
  ),
  {classEnv,_,Defaults4} = Et4(pred:initialEnv()),
  Defaults4 =
    [{tcon,{tycon,"Integer", star}}, {tcon,{tycon,"Double", star}}].

overlap_test() ->
  Pred1 = {isin, "Ord", type:tUnit()},
  Pred2 = {isin, "Ord", type:tChar()},
  false = pred:overlap(Pred1, Pred2),
  true  = pred:overlap(Pred1, Pred1).

% 7.3 Entailment

bySuper_test() ->

  C1 = pre:fold_left1(fun pred:addEnv/2,
    [
      pred:addClass("Eq", []),
      pred:addClass("Ord",["Eq"])
    ]),
  C2 = pred:addEnv(
    pred:addClass("Eq", []),
    pred:addClass("Ord",["Eq"])
  ),
  C2_ = C2(pred:initialEnv()),
  C2_ = C1(pred:initialEnv()),

  Insts = pred:exampleInsts(),
  Preds = pred:bySuper(
    Insts(pred:initialEnv()),
    {isin, "Num",  {tvar,{tyvar,"a", star}}}
  ),
  Preds = [
    {isin, "Num",  {tvar,{tyvar,"a", star}}},
    {isin, "Eq",   {tvar,{tyvar,"a", star}}},
    {isin, "Show", {tvar,{tyvar,"a", star}}}
  ].

byInst_test() ->
  Insts = pred:exampleInsts(), 
  Preds = pred:byInst(
    Insts(pred:initialEnv()),
    {isin, "Num", {tvar,{tyvar,"a", star}}}),
  Preds = ng.

entail_test() ->
  P = {isin, "Num", {tvar,{tyvar,"a", star}}},
  Ps = [P],
  Insts = pred:exampleInsts(),
  Result = pred:entail(Insts(pred:initialEnv()), Ps, P),
  Result = true.


% 7.4 Context Reduction

inHnf_test() ->
  R = pred:inHnf({isin, "Num", {tvar,{tyvar,"a", star}}}),
  R = true,
  R2 = pred:inHnf({isin, "Num", type:tInt()}),
  R2 = false.

toHnfs_test() ->
  Preds = [{isin, "Num", {tvar,{tyvar,"a", star}}}],
  Preds2 = pred:toHnfs(pred:initialEnv(), Preds),
  Preds2 = [{isin, "Num", {tvar,{tyvar,"a", star}}}].

toHnf_test() ->
  Pred = {isin, "Num", {tvar,{tyvar,"a", star}}},
  Preds = pred:toHnf(pred:initialEnv(), Pred),
  Preds = [{isin, "Num", {tvar,{tyvar,"a", star}}}].

simplify_test() ->
  Pred = {isin, "Num", {tvar,{tyvar,"a", star}}},
  Preds = [Pred],
  Insts = pred:exampleInsts(),
  Preds2 = pred:simplify(Insts(pred:initialEnv()), Preds),
  Preds2 = [{isin, "Num", {tvar,{tyvar,"a", star}}}].

reduce_test() ->
  Pred = {isin, "Num", {tvar,{tyvar,"a", star}}},
  Preds = [Pred],
  Insts = pred:exampleInsts(),
  Preds2 = pred:reduce(Insts(pred:initialEnv()), Preds),
  Preds2 = [{isin, "Num", {tvar,{tyvar,"a", star}}}].

scEntail_test() ->
  Pred = {isin, "Num", {tvar,{tyvar,"a", star}}},
  Preds = [Pred],
  Insts = pred:exampleInsts(),
  Result = pred:scEntail(Insts(pred:initialEnv()), Preds, Pred),
  Result = true.
