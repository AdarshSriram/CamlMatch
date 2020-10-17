open OUnit2
(* open Store *)

let store_tests = [

]

let suite = 
  "test suite for Store module" >::: List.flatten [
    store_tests;
  ]

let _ = run_test_tt_main suite