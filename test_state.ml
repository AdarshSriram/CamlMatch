open OUnit2
open State
open Yojson.Basic.Util
open Client


(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^  s ^ "\""

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

let get_users_test 
    (name : string) 
    (store : state) 
    (res : Client.uid list) = 
  name >:: (fun _ -> 
      assert_equal res (get_users store) ~printer: (pp_list pp_string) )

let add_user_test 
    (name : string) 
    (store : state) 
    (uid : Client.uid) 
    (user : Yojson.Basic.t) 
    (res : state) = 
  name >:: (fun _ -> 
      assert_equal (get_users res) (get_users(test_add_user store uid user)))

let get_u_by_id_test 
    (name : string) 
    (store : state) 
    (id : Client.uid)  
    (res : Client.t) = 
  name >:: (fun _ -> 
      assert_equal res (get_user_by_id store id))

let validate_user_exn_test 
    (name : string) 
    (st : state) 
    (uname : string) 
    (pass : string) = 
  name >:: (fun _ -> 
      assert_raises (State.InvalidUser) 
        (fun () -> validate_user st name pass))

let user_can_sign_up_test 
    (name : string) 
    (st : state) 
    (uname : string) 
    (res : bool) = 
  name >:: (fun _ -> 
      assert_equal res (user_can_sign_up st uname))

let get_admins_test 
    (name : string) 
    (st : state) 
    (res : Admin.aid list) = 
  name >:: (fun _ -> 
      assert_equal res (get_admins st) ~printer: (pp_list pp_string) )

let add_admin_test 
    (name : string) 
    (st : state) 
    (aid : Admin.aid) 
    (admin : Yojson.Basic.t) 
    (res : state) = 
  name >:: (fun _ -> 
      assert_equal (get_admins res) (get_admins (test_add_admin st aid admin)))

let get_a_by_id_test 
    (name : string) 
    (st : state) 
    (id : Admin.aid) 
    (res : Admin.t) = 
  name >:: (fun _ -> 
      assert_equal res (get_admin_by_id st id))

let validate_admin_exn_test 
    (name : string) 
    (st : state) 
    (aname : string) 
    (pass : string) = 
  name >:: (fun _ -> 
      assert_raises (State.InvalidUser) 
        (fun () -> validate_user st aname pass))

let admin_can_sign_up_test 
    (name : string) 
    (st : state) 
    (aname : string) 
    (res : bool) = 
  name >:: (fun _ -> 
      assert_equal res (admin_can_sign_up st aname))

let test_state = State.get_state "test_jsons/DummyUsers.json" 
    "test_jsons/DummyAdmins.json"
let empty_state = State.init_state ()

let u5 = Client.to_json (Client.make_user "user 5" "123456" "5")
let u5_encypt = Client.make_user "user 5" "390276637" "5"
let add_user_state = State.get_state "test_jsons/AddUser.json" 
    "test_jsons/DummyAdmins.json"
let u1 = Client.make_user "user 1" "876021969" "1"
let update_u1 = Client.update_prefs u1 [ ("q1", "0"); ("q2", "1")]; u1

let a3 = Admin.to_json (Admin.make_admin "3" "admin 3" "257709571")
let add_admin_state = State.get_state "test_jsons/DummyUser.json" 
    "test_jsons/AddAdmin.json"

let a0 = Admin.make_admin "0" "admin 0" "257709571"


let state_tests = [
  get_users_test "Get users from DummyUsers" test_state 
    ["0"; "1"; "2"; "3"; "4"];
  get_users_test "Get users from empty state" empty_state [];

  add_user_test "Add user 5" test_state "5" u5 add_user_state;
  get_u_by_id_test "Get user 5" add_user_state "5" u5_encypt;

  "Validate user1 login info" >:: (fun _ -> 
      assert_equal (validate_user test_state "user 1" "876021969") 
        (get_user_by_id test_state "1"));

  validate_user_exn_test "Valid user but invalid pass raises exn" 
    test_state "user 1" "pass2";
  validate_user_exn_test "Invalid user name raises exn" 
    test_state "user 6" "pass1";

  user_can_sign_up_test "user 6 can sign up" test_state "user 6" true;
  user_can_sign_up_test "user 1 cannot sign up" test_state "user 1" false;

  "Replace user1 with new prefs" >:: (fun _ -> 
      assert_equal (replace_user test_state update_u1) 
        (State.get_state "test_jsons/ReplaceUser.json" 
           "test_jsons/DummyAdmins.json"));

  (* testing admins *)
  get_admins_test "Get admins from DummyAdmins" test_state 
    ["0"; "1"];
  get_admins_test "Get admins from empty state" empty_state 
    [];
  add_admin_test "Add admin 3" test_state "3" a3 add_admin_state;
  get_a_by_id_test "Get admin 0" test_state "0" a0;
  "Validate admin0 login info" >:: (fun _ -> 
      assert_equal (validate_admin test_state "admin 0" "257709571") 
        (get_admin_by_id test_state "0"));

  validate_admin_exn_test "Valid name but invalid pass raises exn" 
    test_state "admin 1" "pass2";
  validate_admin_exn_test "Invalid name raises exn" 
    test_state "admin 6" "pass1";

  admin_can_sign_up_test "admin 4 can sign up" test_state "admin 4" true;
  admin_can_sign_up_test "admin 1 cannot sign up" test_state "admin 0" false;
]

let suite = 
  "test suite for state module" >::: List.flatten [
    state_tests;
  ]

let _ = run_test_tt_main suite