{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.HUnit
import E

main::IO ()
main = do
  runTestTT $ TestList
    [ 
      "V 1" ~:
        V 1 ~=? V 1,
      "Add (V 1) (V 2)" ~:
        Add (V 1) (V 2) ~=? Add (V 1) (V 2),
      "eval 1 + 2" ~:
        3 ~=? eval (Add (V 1) (V 2)),
      "eval2 [1] + [2]" ~:
        [1,2] ~=? eval2 (Add (V [1]) (V [2])),
      "eval3 1 + 2" ~:
        (Add (V 1) (V 2)) ~=? eval3 (Add (V 1) (V 2)),
      "eval4 1 + 2" ~:
        3 ~=? eval4 (Add (V 1) (V 2)),
      "ev E Integer" ~:
        15 ~=? ev((Add (V 5) (V 10)) :: E Integer),
      "kk E Integer" ~:
        31 ~=? kk((Add (V 10) (V 20)) :: E Integer),

      "ev Integer" ~:
        1 ~=? ev(1 :: Integer),

      "kk Integer" ~:
        2 ~=? kk(1 :: Integer)
    ]
  return ()
