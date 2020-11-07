open OUnit2
open Survey


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

let survey1 = Survey.from_json (Yojson.Basic.from_file "survey1.json")

let empty = {|{"questions": [
  ]
  }|}

let empty_survey = Survey.from_json (Yojson.Basic.from_string empty)

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


]

let suite = 
  "test suite for Survey module" >::: List.flatten [
    survey_tests;
  ]

let _ = run_test_tt_main suite