open OUnit
open Thih
let _ =
  run_test_tt_main ("suite">:::[

    "enumId 1">:: begin fun() ->
      assert_equal (Id.enumId 1) "v1";
    end;

    "enumId 33">:: begin fun() ->
      assert_equal (Id.enumId 33) "v33";
    end
  ])
