open OUnit2
(* open Server *)

let server_tests = [

]

let suite = 
  "test suite for Server module" >::: List.flatten [
    server_tests;
  ]

let _ = run_test_tt_main suite