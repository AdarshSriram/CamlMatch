open OUnit2

let get_id_test 
    (name : string)
    (t : Admin.t) 
    (expected_output : Admin.aid) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Admin.get_aid t) 
        ~printer: (fun x -> x))

let get_name_test 
    (name : string)
    (t : Admin.t) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Admin.get_name t) 
        ~printer: (fun x -> x))

let get_login_test 
    (name : string)
    (t : Admin.t) 
    (expected_output : string*string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Admin.get_login t))

let a1 = Admin.make_admin "1" "ad1" "p1" 
let a2 = Admin.make_admin "2" "ad2" "p2"

let p1_encrypt = Admin.encrypt "change1"

let j = Admin.to_json a1
let a1_fromj = Admin.read_json j

let admin_tests = [
  get_id_test "Ad1 id is 1" a1 "1";

  get_id_test "Ad2 id is 2" a2 "2";
  get_name_test "Ad2 name is ad2" a2 "ad2";
  get_login_test "Ad2 login is ad2*p2" a2 ("ad2", "p2");

  (*Testing change pword *)
  get_login_test "Change Ad1 pword to change1" 
    (Admin.update_pword a1 p1_encrypt; a1) ("ad1", p1_encrypt);

  (*Testing to and from json *)
  get_id_test "Json A1 id is 1" a1_fromj "1";
  get_name_test "Json A1 name is ad1" a1_fromj "ad1";
  get_login_test "Json A1 login is ad1*p1" a1_fromj ("ad1", "p1");

]

let suite = 
  "test suite for Admin module" >::: List.flatten [
    admin_tests;
  ]

let _ = run_test_tt_main suite