open OUnit2
open Client 

let get_id_test 
    (name : string)
    (t : Client.t) 
    (expected_output : Client.uid) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Client.get_uid t))

let get_name_test 
    (name : string)
    (t : Client.t) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Client.get_name t))

let get_login_test 
    (name : string)
    (t : Client.t) 
    (expected_output : string*string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Client.get_login t))

let get_pref_test 
    (name : string)
    (t : Client.t) 
    (expected_output : (string*string) list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Client.get_preferences t))

let get_matches_test 
    (name : string)
    (t : Client.t) 
    (expected_output : (Client.uid*float) list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Client.get_matches t))

let get_notifs_test 
    (name : string)
    (t : Client.t) 
    (expected_output : (Client.uid*string) list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Client.get_notifs t))

let get_logins_test 
    (name : string)
    (t : Client.t) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Client.get_logins t))

let user_of_id_test
    (name : string)
    (id : Client.uid) 
    (lst : Client.t list)
    (expected_output : Client.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Client.user_of_uid id lst))

let user_of_id_raises_test 
    (name : string) 
    (id : Client.uid) 
    (lst : Client.t list) : test = 
  name >:: (fun _ -> 
      assert_raises (Client.UserNotFound id) 
        (fun () -> Client.user_of_uid id lst))

let u1 = Client.make_user "user 1" "pass1" "1" 
let u2 = Client.make_user "user 2" "pass2" "2"
let u3 = Client.make_user "user 3" "pass1" "3"
let u4 = Client.make_user "user 4" "pass4" "4"

let encrypt_p1 = Client.encrypt "change1"

let j = Client.to_json u1
let u1_fromj = Client.read_json j

let client_tests = [
  (* Testing make_user *)
  get_id_test "User id of u1 is 1" u1 "1"; 
  get_name_test "Name of u1 is user 1" u1 "user 1";
  get_login_test "Creditials is user 2 pass2" u2 ("user 2", "pass2");
  get_pref_test "Preferences of user 1 is []" u1 [];
  get_matches_test "User 1 has no matches" u1 [];
  get_logins_test "User 1 has 0 logins" u1 0;

  (*Testing mutable password *)
  get_login_test "Change pword for u1" (Client.update_pword u1 encrypt_p1; u1)
    ("user 1", encrypt_p1);

  get_pref_test "Updated U1 preferences are [3;1]" 
    (Client.update_prefs u2 [("q1", "2"); ("q2", "1")]; u2) 
    [("q1", "2"); ("q2", "1")];

  get_matches_test "Updated U3 matches is [u2]" 
    (Client.update_matches u3 [("2", 0.565)]; u3) ["2", 0.565];
  get_matches_test "Two matches in U2 matches" 
    (Client.update_matches u2 [("1", 0.565); ("4", 0.98)]; u2) 
    [("1", 0.565); ("4", 0.98)];
  get_matches_test "User 4 has no matches" 
    (Client.update_matches u4 [("1", 0.565); ("2", 0.98)]; 
     Client.update_matches u4 []; u4) 
    [];

  get_notifs_test "U1 has no notifications" u1 [];
  get_notifs_test "U2 has one notification" 
    (Client.update_notifs u2 "1" "hello"; u2) [("1","hello")];
  get_notifs_test "U3 has two notifications" 
    (Client.update_notifs u3 "4" "bye"; 
     Client.update_notifs u3 "1" "goodbye"; u3) 
    [("1", "goodbye"); ("4","bye")];

  user_of_id_test "User of id 2 is u2" "2" [u1; u2; u3] u2;
  user_of_id_raises_test "UserNotFound raised with searching for id 4" 
    "4" [u1; u2; u3];

  get_logins_test "U2 has logged in once" (Client.incr_logins u2; u2) 1;
  get_logins_test "U3 has logged in twice" 
    (Client.incr_logins u3; Client.incr_logins u3; u3) 2;

  (*Testing to and from json *)
  get_id_test "Json u1 id is 1" u1_fromj "1";
  get_name_test "Json u1 name is user 1" u1_fromj "user 1";
  get_login_test "Json u1 login is user 1*pass1" u1_fromj ("user 1", "pass1");
]

let suite = 
  "test suite for Client module" >::: List.flatten [
    client_tests;
  ]

let _ = run_test_tt_main suite