import Test.HUnit
import B

main::IO ()
main = do
  runTestTT $ TestList
    [ 
      "test f" ~:   1 ~=? f,
      "test g" ~:   [1,2,3] ~=? g
    ]
  return ()
