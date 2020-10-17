open OUnit2
(* open Client *)

let client_tests = [

]

let suite = 
  "test suite for Client module" >::: List.flatten [
    client_tests;
  ]

let _ = run_test_tt_main suite