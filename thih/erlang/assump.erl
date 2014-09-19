% 9 Assumptions

-module(assump).
-export([
  assump/2,
  assumpApply/2,
  assumpTv/1,
  assumpsApply/2,
  assumpsTv/1,
  find/2
]).

assump(Id, Scheme) -> {assump, Id, Scheme}.

assumpApply(Subst, Assump) ->
  {assump, I, Scheme} = Assump,
  assump(I, scheme:schemeApply(Subst, Scheme)).

assumpTv(Assump) ->
  {assump, _, Scheme} = Assump,
  scheme:schemeTv(Scheme).

assumpsApply(Subst, Assumps) ->
  subst:listApply(fun assumpApply/2, Subst, Assumps).

assumpsTv(Assumps) ->
  subst:listTv(fun assumpTv/1, Assumps).


find(Id, Assumps) ->
  case lists:keyfind(Id,2, Assumps) of
    {assump, _, Scheme} -> Scheme;
    _ -> throw(string:concat("not found ", Id))
  end.

