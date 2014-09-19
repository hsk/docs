-module(t03_kind_tests).
-include_lib("eunit/include/eunit.hrl").

kind_test() ->
  Star = kind:star(),
  Kfun1 = kind:kfun(kind:star(), kind:star()),
  Kfun2 = kind:kfun(kind:star(), kind:kfun(kind:star(), kind:star())),
  Kfun3 = kind:kfun(kind:star(), kind:kfun(kind:star(), kind:kfun(kind:star(), kind:star()))),
  Kfun4 = kind:kfun(kind:kfun(kind:star(), kind:star()), kind:kfun(kind:star(), kind:star())),
  Kfun5 = kind:kfun(kind:star(), kind:kfun(kind:kfun(kind:star(), kind:star()), kind:star())).
