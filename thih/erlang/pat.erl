% 11.2 Patterns
-module(pat).
-export([
  pvar/1,
  pwildcard/0,
  pas/2,
  plit/1,
  pnpk/2,
  pcon/2,
  tiPat/2,
  tiPats/2
]).

pvar(Id) -> {pvar, Id}.
pwildcard() -> pwildcard.
pas(Id,Pat) -> {pas, Id, Pat}.
plit(Lit) -> {plit, Lit}.
pnpk(Id,Long) -> {pnpk, Id, Long}.
pcon(Assump, Pats) -> {pcon, Assump, Pats}.

tiPat(Ti, {pvar, I}) ->
  T = timonad:newTVar(Ti,kind:star()),
  {[], [assump:assump(I, toScheme(T))], T};
tiPat(Ti, pwildcard) -> ([], [], timonad:newTVar(Ti,kind:star()));
tiPat(Ti, {pas, I, Pat}) ->
  {Ps, As_, T} = tiPat(Ti, Pat),
  {Ps, [assump:assump(I, scheme:toScheme(T)) | As_], T};
tiPat(Ti, {plit, L}) ->
  {Ps, T} = lit:tiLit(Ti, L),
  {Ps, [], T};
tiPat(Ti, {pnpk, i, k}) ->
  T = type:newTVar(Ti, kind:star()),
  {[pred:isIn("Integral", T)], [assump:assump(I, scheme:toScheme(T))], T};
tiPat(Ti, {pcon, {assump, I, Sc}, Pats}) ->
  {Ps, As_, Ts} = tiPats(Ti, Pats),
  T_ = timonad:newTVar(Ti, kind:star()),
  {qual, Qs, T} = timonad:freshInst(Ti, Sc),
  timonad:unify(Ti, T, lists:foldr(fun(A, B)-> type:fn(A,B) end, T_, TS)),
  {lists:append(Ps,Qs), As_, T_}.

tiPats(Ti, Pats) ->
  (Pss, Ass, Ts) = pre:split3(lists:map(fun(Pat)->tiPat(Ti,Pat)end,Pats)),
  {lists:concat(Pss), lists:concat(Ass), Ts}
