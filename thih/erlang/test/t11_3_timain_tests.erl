-module(t11_3_timain_tests).
-include_lib("eunit/include/eunit.hrl").

ambiguities_test() ->
  Tvs = [type:tyvar("a", star)],
  Preds = [pred:isin("Num", type:tInt()), pred:isin("B", type:tInt())],
  Ambs = timain:ambiguities(Tvs, Preds),

  Ambs = [].

numClasses_test() ->
  ["Num", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"]
  = timain:numClasses().

stdClasses_test() ->
  ["Eq", "Ord", "Show", "Read", "Bounded", "Enum",
    "Ix", "Functor", "Monad", "MonadPlus", "Num", "Integral",
    "Floating", "Fractional", "Real", "RealFloat", "RealFrac"]
  = timain:stdClasses().
  
test_test() ->
  Tv = type:tyvar("a", star),
  Preds = [pred:isin("Num", type:tInt()), pred:isin("B", type:tInt())],
  Amb = {Tv, Preds},
  Amb = {Tv, Preds}.
