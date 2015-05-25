# fixity 演算子優先順位

prefix 61 ~
infixl 60 * / % mod
infixl 50 + -
infixl 41 asl asr
infixl 41 lsl lsr
infix 40 < <= > >=
infixr 40 :: @
infix 30 = == != <>
infixl 21 &&
infixl ( && ) andalso land
infixl 20 ||
infixl ( || ) orelse lor lxor
infixr 10 ->
infix 0 := // HX: assign
infix 0 :=: // HX: exchange
infixl 0 << (* g0int_asl, g0uint_lsl *)
infixr 0 >> (* g0int_asr, g0uint_lsr *)
prefix 0 ++ -- // inc and dec
prefix 0 !++ --! // getinc and decget
infixr 0 =++ --= // setinc and decset
infix 0 :+= :-= :*= :/= // x:=x+a, x:=x-a, ...
infix 0 :=+ :=- :=* :=/ // x:=a+x, x:=a-x, ...
