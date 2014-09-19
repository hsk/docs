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

tiLit(Ti, {litChar, _}) -> {[],type:tChar()};
tiLit(Ti, {litInt, _}) ->
  V = type:newTVar(Ti, kind:star()),
  {[pred:isIn("Num", V)], V};
tiLit(Ti, {litStr,_}) -> {[], type:tString()};
tiLit(Ti, {litRat,_}) ->
  V = type:newTVar(Ti, kind:star()),
  {[pred:isIn("Fractional", V)], V}.
