open OUnit2
open Survey

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_key pp_val lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [(x,y)] -> acc ^ "(" ^ pp_key x ^ ", " ^ pp_val y ^ ")"
      | (x,y) :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ "(" ^ pp_key x ^ ", " ^ pp_val y ^ "); ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

let float_close_enough f1 f2 = (f1 -. f2) < 0.0001

let list_close_enough l1 l2 = 
  let lens = List.length l1 = List.length l2 in 
  let rec helper acc l1 l2 = 
    match l1 with 
    | [] -> acc 
    | (x1, y1) :: t1 -> begin 
        match l2 with 
        | [] -> false 
        | (x2, y2) :: t2 -> begin 
            helper (acc && x1 = x2 && (float_close_enough y1 y2)) t1 t2
          end
      end 
  in 
  lens && helper true l1 l2

let survey1 = Survey.from_json (Yojson.Basic.from_file "survey1.json")

let empty_survey = Survey.from_json 
    (Yojson.Basic.from_file "test_jsons/EmptySurvey.json")

let empty_state = State.init_state ()

let pref_state = State.get_state "test_jsons/PrefUsers.json" 
    "test_jsons/DummyAdmins.json"
let user_uids = State.get_users pref_state

let rec get_user_by_id id = function 
  | [] -> failwith "No such user"
  | h :: t -> if Client.get_uid h = id then h else get_user_by_id id t

let user_recs = State.get_user_recs pref_state

let u1_prefs = get_user_by_id "1" user_recs
let u2_prefs = get_user_by_id "2" user_recs
let u3_prefs = get_user_by_id "3" user_recs
let u4_prefs = get_user_by_id "4" user_recs

let question_list_test 
    (name : string) 
    (input: Survey.t) 
    (expected_output : Survey.q list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Survey.question_list input))

let answer_list_test 
    (name : string) 
    (s: Survey.t) 
    (id: Survey.qid)
    (expected_output : Survey.a list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Survey.answer_list s id))

let check_ans_test 
    (name : string) 
    (s: Survey.t) 
    (id: Survey.qid)
    (aid: Survey.aid)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal (expected_output) (Survey.check_ans s id aid))

let check_ans_exn_test 
    (name : string) 
    (s : Survey.t) 
    (qid : Survey.qid) 
    (aid : Survey.aid) = 
  name >:: (fun _ -> 
      assert_raises (Failure "Invalid entry") 
        (fun () -> Survey.check_ans s qid aid))

let type_of_question_test 
    (name : string) 
    (s: Survey.t) 
    (id: Survey.qid)
    (expected_output : Survey.qtype) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Survey.type_of_question s id ))

let match_score_test 
    (name : string) 
    (s: Survey.t) 
    (p1: (qid * string) list)
    (p2: (qid * string) list)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Survey.match_score p1 p2 (Survey.question_list s))
        ~printer: string_of_float
        ~cmp: float_close_enough)

let compile_matches_test 
    (name : string) 
    (c: Client.t) 
    (st: State.state)
    (s: Survey.t)
    (expected_output : (Client.uid*float) list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Survey.compile_matches c st s)
        ~printer: (pp_list (fun x -> x) string_of_float)
        ~cmp: list_close_enough)

let quest_hist_exn_test 
    (name : string) 
    (qid : Survey.qid) 
    (st : State.state) 
    (s : Survey.t)  = 
  name >:: (fun _ -> 
      assert_raises (Failure "Invalid question id") 
        (fun () -> Survey.question_histogram qid st s))

let hist_values_test 
    (name : string) 
    (st : State.state) 
    (qid : string) 
    (ulist : string list)
    (survey : t) 
    (expected : int list) = 
  name >:: (fun _ -> 
      assert_equal expected (test_hist_values st qid ulist survey))

let survey_tests = [
  question_list_test "Empty survey returns empty list" empty_survey [];
  question_list_test "Survey 1 returns q list" survey1 (Survey.get_q_list ());

  answer_list_test "Survey 1 qid q3 returns Appel and RPCC" survey1 "q3"
    (Survey.get_q3_ans ());

  "find q test; qid of q4 returns question 4" >:: (fun _ -> 
      assert_equal (Survey.get_q4_rec ()) (Survey.find_q (Survey.get_q_list()) 
                                             "q4"));

  "get_qid test; question 4 has qid of q4" >:: (fun _ -> 
      assert_equal "q4" (Survey.get_qid (Survey.get_q4_rec ())));

  check_ans_test "answer 0 is valid for question 1" survey1 "q1" "0" "0";
  check_ans_test "answer 3 is valid for question 2" survey1 "q2" "3" "3";

  check_ans_exn_test "answer 4 is invalid for question1" survey1 "q1" "4";
  check_ans_exn_test "answer (-1) is invalid for question1" survey1 "q1" "-1";

  type_of_question_test "question 1 has an Rng type" survey1 "q1" 
    (Survey.get_q1_type ());
  type_of_question_test "question 2 has an Opt type" survey1 "q2" 
    (Survey.get_q2_type ());

  match_score_test "u1 u2 should be highest match" survey1
    (Client.get_preferences u1_prefs) (Client.get_preferences u2_prefs) 
    ((7. /. 3.)/. 4.);
  match_score_test "u1 u3 should be less than u2" survey1
    (Client.get_preferences u1_prefs) (Client.get_preferences u3_prefs) 
    ((1. /. 3.)/. 4.);
  match_score_test "u1 u4 should be lowest" survey1
    (Client.get_preferences u1_prefs) (Client.get_preferences u4_prefs) 0.;

  compile_matches_test "empty state should return empty list" u1_prefs 
    empty_state survey1 [];
  compile_matches_test "u1 should have u2 as a match" u1_prefs 
    pref_state survey1 [("2", (7. /. 3.) /. 4.)];
  compile_matches_test "u4 should have u2 and u3 as a match" u4_prefs 
    pref_state survey1 [("2", (1. /. 3.) /. 4.); ("3", (1. /. 3.) /. 4.)];

  (* testing histogram *)
  hist_values_test "q1 with pref_state" pref_state "q1" user_uids 
    survey1 [1;1;0;2];
  hist_values_test "q2 with pref_state" pref_state "q2" user_uids 
    survey1 [1;3;0;0];
  hist_values_test "q3 with pref_state" pref_state "q3" user_uids 
    survey1 [2;2];
  hist_values_test "q4 with pref_state" pref_state "q4" user_uids 
    survey1 [1;1;1;1];

  (*testing question_histogram and answer_list failure *)
  quest_hist_exn_test "question histogram with qid 7 fails" "q7" pref_state 
    survey1;
  quest_hist_exn_test "question histogram with qid 0 fails" "q0" pref_state 
    survey1;
]

let suite = 
  "test suite for Survey module" >::: List.flatten [
    survey_tests;
  ]

let _ = run_test_tt_main suite