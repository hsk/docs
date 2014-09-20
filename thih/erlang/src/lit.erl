% // 11.1 Literals
-module(lit).
-export([
  litInt/1,
  litChar/1,
  litRat/1,
  litStr/1,
  tiLit/2
]).

litInt(Long) -> {litInt, Long}.
litChar(Char) -> {litInt, Char}.
litRat(Double) -> {litInt, Double}.
litStr(String) -> {litInt, String}.

tiLit(_, {litChar, _}) -> {[],type:tChar()};
tiLit(Ti, {litInt, _}) ->
  V = timonad:newTVar(Ti, kind:star()),
  {[pred:isin("Num", V)], V};
tiLit(_, {litStr,_}) -> {[], type:tString()};
tiLit(Ti, {litRat,_}) ->
  V = type:newTVar(Ti, kind:star()),
  {[pred:isin("Fractional", V)], V}.
