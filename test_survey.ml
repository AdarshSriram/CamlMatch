open OUnit2
open Survey


let survey_test 
    (name : string) 
    (input: Survey.t list) 
    (expected_output : Survey.t list) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output input)

let survey_tests = [
  survey_test "Empty survey" Survey.empty [];
  survey_test "Add one question" 
    (Survey.add_question [] Survey.q1) [Survey.q1];
  survey_test "Add repeated question" 
    (Survey.add_question Survey.question_list Survey.q1) Survey.question_list;
  survey_test "Remove question"
    (Survey.rem_question "RPCC or Appel?" Survey.question_list) 
    [Survey.q1;Survey.q2;Survey.q3];
  survey_test "Remove nonexistent question" 
    (Survey.rem_question "Hi" Survey.question_list) Survey.question_list;
]

let suite = 
  "test suite for Survey module" >::: List.flatten [
    survey_tests;
  ]

let _ = run_test_tt_main suite