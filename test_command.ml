open OUnit2
open Command

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

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

(** [pp_command] pretty-prints the command type, then uses [pp_elt]
    to pretty-print the location if command is a Go. *)
let pp_command = function
  | Send (x, y) -> "Send: " ^ pp_list pp_string [x;y]
  | Quit -> "Quit"
  | View x -> "View: " ^ pp_string x
  | UReset x -> "UReset: " ^ pp_string x
  | CHelp x -> "CHelp"

(** [pp_command] pretty-prints the command type, then uses [pp_elt]
    to pretty-print the location if command is a Go. *)
let pp_acommand = function
  | Hist x -> "Hist: " ^ x
  | Quit -> "Quit"
  | AView x -> "AView: " ^ pp_string x
  | AReset x -> "AReset: " ^ pp_string x
  | AHelp x -> "AHelp"
  | Dist (x, y) -> "Dist" ^ pp_list pp_string [x;y]
  | Graph -> "Graph"

let parse_user_test 
    (name: string)
    (user: Client.t)
    (st: State.state)
    (str: string)
    (expected_value: command) =
  name >:: (fun _ -> 
      assert_equal ~printer:(pp_command)
        expected_value (parse_user user str st))

let parse_user_error
    (name: string)
    (user: Client.t)
    (st: State.state)
    (str: string)
    (expected_value: exn) =
  name >:: (fun _ -> 
      assert_raises expected_value (fun () -> parse_user user str st))

let pref_state = State.get_state "test_jsons/PrefUsers.json" 
    "test_jsons/DummyAdmins.json"
let u1 = State.get_user_by_id pref_state "1"

let parse_admin_test 
    (name: string)
    (admin: Admin.t)
    (st: State.state)
    (str: string)
    (expected_value: acommand) =
  name >:: (fun _ -> 
      assert_equal ~printer:(pp_acommand)
        expected_value (parse_admin admin str st))

let parse_admin_error
    (name: string)
    (admin: Admin.t)
    (st: State.state)
    (str: string)
    (expected_value: exn) =
  name >:: (fun _ -> 
      assert_raises expected_value (fun () -> parse_admin admin str st))

let a1 = State.get_admin_by_id pref_state "0"

let command_tests = [
  parse_user_test "Parse send" u1 pref_state "send user2 hello" 
    (Send ("user2", "hello "));
  parse_user_test "Parse view" u1 pref_state "view matches" 
    (View "matches");
  parse_user_test "Parse ureset" u1 pref_state "reset password"
    (UReset "password");
  parse_user_test "Parse quit" u1 pref_state "quit" Quit;
  parse_user_test "Parse chelp" u1 pref_state "help" (CHelp command_list);

  (* testing errors *)
  parse_user_error "malformed send" u1 pref_state "send user2" Malformed;
  parse_user_error "nouserfound send" u1 pref_state "send xyz hello"
    Malformed;
  parse_user_error "malformed view" u1 pref_state "view sdjk" Malformed;
  parse_user_error "malformed reset" u1 pref_state "reset" Malformed;
  parse_user_error "malformed quit" u1 pref_state "qit" Malformed;
  parse_user_error "malformed help" u1 pref_state "help me" Malformed;

  (* testing admin commands *)
  parse_admin_test "parse hist" a1 pref_state "hist q1" (Hist "q1");
  parse_admin_test "parse graph" a1 pref_state "graph" Graph;
  parse_admin_test "parse dist" a1 pref_state "dist user1 user2" 
    (Dist ("user1", "user2"));
  parse_admin_test "parse reset" a1 pref_state "reset password" 
    (AReset "password");
  parse_admin_test "parse quit" a1 pref_state "quit" Quit;
  parse_admin_test "parse help" a1 pref_state "help" (AHelp acommand_list);
  parse_admin_test "parse view" a1 pref_state "view 1" (AView "1");

  (* testing admin errors *)
  parse_admin_error "malformed hist" a1 pref_state "hist q1 q2 q3" Malformed;
  parse_admin_error "malformed graph" a1 pref_state "graph xx" Malformed;
  parse_admin_error "malformed dist" a1 pref_state "dist user1 user2 u3" 
    Malformed;
  parse_admin_error "malformed reset" a1 pref_state "reset paword" 
    Malformed;
  parse_admin_error "malformed quit" a1 pref_state "quit xx" Malformed;
  parse_admin_error "malformed help" a1 pref_state "halp" Malformed;
  parse_admin_error "maleformed view" a1 pref_state "view usr1" Malformed;
]

let suite = 
  "test suite for command module" >::: List.flatten [
    command_tests;
  ]

let _ = run_test_tt_main suite