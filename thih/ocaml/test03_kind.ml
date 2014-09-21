open OUnit
open Thih
open Thih.Kind
let _ =
  run_test_tt_main ("kind">:::[
    "kind">:: begin fun() ->
      let _ = Star in
      let _ = Kfun(Star, Star) in
      let _ = Kfun(Star, Kfun(Star, Star)) in
      let _ = Kfun(Star, Kfun(Star, Kfun(Star, Star))) in
      let _ = Kfun(Kfun(Star, Star), Kfun(Star, Star)) in
      let _ = Kfun(Star,Kfun(Kfun(Star, Star), Star)) in
      ()
    end
  ])
