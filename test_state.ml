open OUnit2
open State
open Yojson.Basic.Util
open Client

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ string_of_int s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

let get_users_test name store res = 
  name >:: (fun _ -> 
      assert_equal res (get_users store) )

let add_user_test name store uid user res = 
  name >:: (fun _ -> 
      assert_equal (get_users res) (get_users(testing_add_user store uid user)))

let get_u_by_id_test name store id res = 
  name >:: (fun _ -> 
      assert_equal res (get_user_by_id store id))

let validate_user_exn_test name st uname pass = 
  name >:: (fun _ -> 
      assert_raises (State.InvalidUser) 
        (fun () -> validate_user st name pass))


let state_tests = [
  get_users_test "Get users" State.test_state ["1";"2";"3";"4"];
  get_users_test "Get users" State.empty_state ["1"];
  add_user_test "Add user 5" State.test_state "5" State.u5 
    {user_list = `Assoc [("5", State.u5);("1", State.u1);("2", State.u2);
                         ("3", State.u3);("4", State.u3)]};
  get_u_by_id_test "Get user 3" State.test_state "3" (read_json State.u3);

  "Validate user 1 pass1" >:: (fun _ -> 
      assert_equal (validate_user  State.test_state "user 1" "pass1") 
        (get_user_by_id State.test_state "1"));

  validate_user_exn_test "Valid user but invalid pass raises exn" 
    State.test_state "user 1" "pass2";
  validate_user_exn_test "Invalid user name raises exn" 
    State.test_state "user 6" "pass1";

]

let suite = 
  "test suite for state module" >::: List.flatten [
    state_tests;
  ]

let _ = run_test_tt_main suite