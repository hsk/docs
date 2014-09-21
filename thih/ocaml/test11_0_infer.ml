open OUnit
open Thih
open Thih.Type
open Thih.Subst
open Thih.Kind
open Thih.Unify
open Thih.Pred
open Thih.Scheme
open Thih.Assump
open Thih.TIMonad
open Thih.Infer
let (=:=) = assert_equal

let _ =
  run_test_tt_main ("id">:::[
  ])
