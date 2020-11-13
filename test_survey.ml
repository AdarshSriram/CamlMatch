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

let empty = {|{"questions": [
  ]
  }|}

let empty_survey = Survey.from_json (Yojson.Basic.from_string empty)

(* in order of closest match to u1 to least close *)
let u1_prefs =  Client.read_json State.pref_1

let u2_prefs = Client.read_json State.pref_2

let u3_prefs = Client.read_json State.pref_3

let u4_prefs = Client.read_json State.pref_4

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
        (Survey.compile_matches c st (Survey.question_list s))
        ~printer: (pp_list (fun x -> x) string_of_float)
        ~cmp: list_close_enough)

let survey_tests = [
  question_list_test "Empty survey returns empty list" empty_survey [];
  question_list_test "Survey 1 returns q list" survey1 Survey.q_list;

  answer_list_test "Survey 1 qid q3 returns Appel and RPCC" survey1 "q3"
    Survey.q3_ans;

  "find q test; qid of q4 returns question 4" >:: (fun _ -> 
      assert_equal Survey.q4_rec (Survey.find_q Survey.q_list "q4"));

  "get_qid test; question 4 has qid of q4" >:: (fun _ -> 
      assert_equal "q4" (Survey.get_qid Survey.q4_rec));

  check_ans_test "answer 0 is valid for question 1" survey1 "q1" "0" "0";
  check_ans_test "answer 3 is valid for question 2" survey1 "q2" "3" "3";

  type_of_question_test "question 1 has an Rng type" survey1 "q1" 
    Survey.q1_type;
  type_of_question_test "question 2 has an Opt type" survey1 "q2" 
    Survey.q2_type;

  match_score_test "u1 u2 should be highest match" survey1
    (Client.get_preferences u1_prefs) (Client.get_preferences u2_prefs) 
    (7. /. 3.);
  match_score_test "u1 u3 should be less than u2" survey1
    (Client.get_preferences u1_prefs) (Client.get_preferences u3_prefs) 
    (1. /. 3.);
  match_score_test "u1 u4 should be lowest" survey1
    (Client.get_preferences u1_prefs) (Client.get_preferences u4_prefs) 0.;

  compile_matches_test "empty state should return empty list" u1_prefs 
    State.empty_state survey1 [];
  compile_matches_test "u1 should have u2 as a match" u1_prefs 
    State.pref_state survey1 [("2", 7. /. 3.)];
  compile_matches_test "u4 should have u2 and u3 as a match" u4_prefs 
    State.pref_state survey1 [("2", 1. /. 3.); ("3", 1. /. 3.)]
]

let suite = 
  "test suite for Survey module" >::: List.flatten [
    survey_tests;
  ]

let _ = run_test_tt_main suite