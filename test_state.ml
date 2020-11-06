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

let u1 = to_json (Client.make_user "user 1" "1" )
let u2 = to_json (Client.make_user "user 2" "2")
let u3 = Client.make_user "user 3" "3" |> to_json
let u4 = Client.make_user "user 4" "4" |> to_json
let u5 = Client.make_user "user 5" "5" |> to_json

let u6 = 
  let user  = (Client.make_user "user 6" "6" ) in 
  Client.update_matches user "2";
  Client.update_matches user "3";
  Client.update_prefs user [("1", "2");("2", "1")]; user 

let test_store = 
  { 
    user_list = `Assoc [("1", u1);("2", u2);("3", u3);("4", u4)];
  }

(*let get_chats_test name store uid res = 
  name >:: (fun _ -> 
      assert_equal res (get_chats store uid ))*)

let get_users_test name store res = 
  name >:: (fun _ -> 
      assert_equal res (get_users store) )

(*let get_users_of_chat_test name store chat res = 
  name >:: (fun _ -> 
      assert_equal res (get_users_of_chat store chat))*)

let add_user_test name store uid user res = 
  name >:: (fun _ -> 
      assert_equal (get_users res) (get_users(add_user store uid user)) )

(*let add_chat_test name store chatid u1 u2 res = 
  name >:: (fun _ -> 
      assert_equal (get_users res) (get_users (add_chat store chatid u1 u2)))*)

let get_u_by_id_test name store id res = 
  name >:: (fun _ -> 
      assert_equal res (get_user_by_id store id))

let store_test name state =
  name >:: (fun _ -> 
      assert_equal state.user_list (store_users state; get_user_data "Users.json"))

let store_tests = [
  (*get_chats_test "Get user 1 chat" test_store "1" 1;*)
  get_users_test "Get users" test_store ["1";"2";"3";"4"];
  add_user_test "Add user 5" test_store "5" u5 
    {user_list = `Assoc [("5", u5);("1", u1);("2", u2);("3", u3);("4", u3)]};
  get_u_by_id_test "Get user 3" test_store "3" (read_json u3);
  store_test "Storing users" (add_user test_store "6" (Client.to_json u6)); 
]

let suite = 
  "test suite for Store module" >::: List.flatten [
    store_tests;
  ]

let _ = run_test_tt_main suite