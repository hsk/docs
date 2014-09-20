open OUnit
open Thih
let _ =
  run_test_tt_main ("suite">:::[

    "union">:: begin fun() ->
      assert_equal (Pre.union [1;2] [2; 3]) [1; 2; 3];
    end;

    "intersect">:: begin fun() ->
      assert_equal (Pre.intersect [1;2] [2; 3]) [2];
    end;

    "union and intersect">:: begin fun() ->
      let a = [5;4;3;2;1] in
      let b = [4;5;6;7] in
      let ab = Pre.union a b in
      assert_equal [3;2;1;4;5;6;7] ab;
      let ab = Pre.intersect a b in
      assert_equal [5;4] ab;
    end;

    "nub">:: begin fun() ->
      let a = [1;1;2;2;3;4;5;1] in
      let a = Pre.nub a in
      assert_equal [5;4;3;2;1] a;
    end;

    "is empty 1">:: begin fun() ->
      let a = [] in
      let a = Pre.isEmpty a in
      assert_equal true a;
    end;

    "is empty 2">:: begin fun() ->
      let a = [1] in
      let a = Pre.isEmpty a in
      assert_equal false a;
    end;

    "fold_left1">:: begin fun() ->
      let a = [1;2;3;4;5;6;7;8;9;10] in
      let a = Pre.fold_left1 begin fun a b ->
        a + b
      end a in
      assert_equal 55 a
    end;

    "deleteFirst">:: begin fun() ->
      let a = [1;2;3;4;3;4;5] in
      let a = Pre.deleteFirst 3 a in
      assert_equal [1;2;4;3;4;5] a;
    end;

    "diff">:: begin fun() ->
      let a = [1;2;3;4] in
      let b = [3;4;5;6] in
      let a = Pre.diff a b in
      assert_equal [1;2] a;
    end;

    "diff 2">:: begin fun() ->
      let a = [1;2;3;4;3;4] in
      let b = [3;4;5;6;4] in
      let a = Pre.diff a b in
      assert_equal [1;2;3] a;
    end;

    "split3">:: begin fun() ->
      let a1 = [1,10,100;2,20,200;3,30,300] in
      let a,b,c = Pre.split3 a1 in
      assert_equal [1;2;3] a;
      assert_equal [10;20;30] b;
      assert_equal [100;200;300] c;
    end
  ])
