open OUnit

let _ =
  run_test_tt_main ("suite">:::[
    "test1">:: begin fun() ->
      assert_equal 100 (Foo.unity 100)
    end;
    "test2">:: begin fun() ->
      assert_equal "x" (Foo.unity "x")
    end
  ])

