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

let u1 = to_json (Client.make_user "user 1" "pass1" "1" )
let u2 = to_json (Client.make_user "user 2" "pass2" "2")
let u3 = Client.make_user "user 3" "pass3" "3" |> to_json
let u4 = Client.make_user "user 4" "pass4" "4" |> to_json
let u5 = Client.make_user "user 5" "pass5" "5" |> to_json

let u6 = 
  let user  = (Client.make_user "user 6" "pass6" "6" ) in 
  Client.update_matches user "2";
  Client.update_matches user "3";
  Client.update_prefs user [("1", "2");("2", "1")]; user 

let test_store = 
  { 
    user_list = `Assoc [("1", u1);("2", u2);("3", u3);("4", u4)];
  }


let get_users_test name store res = 
  name >:: (fun _ -> 
      assert_equal res (get_users store) )


let add_user_test name store uid user res = 
  name >:: (fun _ -> 
      assert_equal (get_users res) (get_users(add_user store uid user)) )


let get_u_by_id_test name store id res = 
  name >:: (fun _ -> 
      assert_equal res (get_user_by_id store id))

(* let store_test name state =
   name >:: (fun _ -> 
      assert_equal (get_user_data "Users.json") (store_users state ).user_list) *)

let validate_user_exn_test name st uname pass = 
  name >:: (fun _ -> 
      assert_raises (State.InvalidUser) 
        (fun () -> validate_user st name pass))


let state_tests = [
  get_users_test "Get users" test_store ["1";"2";"3";"4"];
  add_user_test "Add user 5" test_store "5" u5 
    {user_list = `Assoc [("5", u5);("1", u1);("2", u2);("3", u3);("4", u3)]};
  get_u_by_id_test "Get user 3" test_store "3" (read_json u3);

  (* store_test "Storing users" (add_user test_store "6" (Client.to_json u6)); *)

  "Validate user 1 pass1" >:: (fun _ -> 
      assert_equal (validate_user  test_store "user 1" "pass1") 
        (get_user_by_id test_store "1"));

  validate_user_exn_test "Valid user but invalid pass raises exn" 
    test_store "user 1" "pass2";
  validate_user_exn_test "Invalid user name raises exn" 
    test_store "user 6" "pass1";


]

let suite = 
  "test suite for state module" >::: List.flatten [
    state_tests;
  ]

let _ = run_test_tt_main suite