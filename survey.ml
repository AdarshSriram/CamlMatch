(* TODO: make survey a json file *)
open Yojson.Basic.Util

type qid = string
type aid = string


type qtype = | RNG | OPT

(* q_type is 0 if discrete and 1 if range *)
type t = Yojson.Basic.t 

type a = {
  a_id: aid;
  a_str: string
}

type q = {
  id : qid;
  q_str : string;
  q_type : qtype; 
  ans : a list
}

let from_json json = json

(** [string_of_json s name] converts the value associated with key [name] 
    in the JSON object [s] into a string
    Requires: [s] is a valid JSON representation of a survey *)
let string_of_json s name = 
  s |> member name |> to_string

(** [list_of_json s name] converts the value associated with key [name] 
    in the JSON object [s] into a list
    Requires: [s] is a valid JSON representation of a survey *)
let list_of_json s name = 
  s |> member name |> to_list

(** [to_answer_type a] converts [a] into an answer record *)
let to_answer_type a = {
  a_id = string_of_json a "aid";
  a_str = string_of_json a "answer"
}

(** [qtype_of_string str] converts [str] into a qtype 
    Raises: Failure if str does not equal Rng or Opt *)
let qtype_of_string str = 
  if str = "RNG" then RNG 
  else if str = "OPT" then OPT 
  else failwith "Invalid question type"

(** [to_question_type q] converts [q] into a question record *)
let to_question_type q = {
  id = string_of_json q "id";
  q_str = string_of_json q "question";
  q_type = string_of_json q "qtype" |> qtype_of_string;
  ans = list_of_json q "answers" |> List.map to_answer_type 
}

let question_list s = 
  list_of_json s "questions" |> List.map to_question_type

(** [find_ans_to_q q qlist] returns the answers to the question in [qlist] 
    with id [q]
    Raises: Failure if [q] is not a valid id *)
let rec find_ans_to_q q = function 
  | [] -> failwith "Invalid question id"
  | h :: t -> if h.id = q then h.ans else find_ans_to_q q t

let answer_list s q = 
  let q_list = question_list s in 
  find_ans_to_q q q_list 

(** [get_q q qlist] returns the question from [qlist] with id [q]
    Raises: Failure if [q] is not a valid id *)
let rec get_q q = function 
  | [] -> failwith "Invalid question id"
  | h :: t -> if h.id = q then h else get_q q t 

let find_q qlist q = 
  get_q q qlist 

let get_qid q = q.id

let check_ans s q ans =
  let a_list = answer_list s q in
  let aid_list = List.map (fun a -> a.a_id) a_list in 
  if List.mem ans aid_list then ans
  else failwith "Invalid entry"

(* let  rec same_ans b y1 y2 = 
   match (y1, y2) with
   | ([],[]) -> b
   | (h :: t, a :: b) -> same_ans (h = a) t b
   | ([],_) | (_,[]) -> false

   let same_q q1 q2 =
   if q1.question = q2.question && same_ans true q1.answers q2.answers
   then 0 
   else -1  (-1 is returned to preserve list order) *)

(* did not implement because not necessary right now. 
   add_question will be necessary when admins can create new surveys *)
let add_question lst q =
  failwith "Unimplemented"

let type_of_question s q = 
  let q_list = question_list s in 
  let quest = get_q q q_list in 
  quest.q_type

let score_rng a1 a2 q = 
  let div = List.length q.ans in 
  (float_of_string a1 +. float_of_string a2) /. float_of_int div

let score_opt a1 a2 = 
  if a1 = a2 then 1. else 0.

(* This iterates through preference lists of both users and call
   score_rng or score_opt to calculare the total score *)
let rec score_aux score p1 p2 survey = 
  match p1 with 
  | [] -> score 
  | (qid1, a1) :: t1 -> begin 
      match p2 with 
      | [] -> failwith "Incomplete preferences list"
      | (qid2, a2) :: t2 -> begin 
          if qid1 <> qid2 then failwith "Preference list not in order" 
          else
            let q = get_q qid1 survey in 
            if q.q_type = RNG 
            then score_aux (score +. score_rng a1 a2 q) t1 t2 survey
            else score_aux (score +. score_opt a1 a2) t1 t2 survey
        end
    end

(* returns match score of 2 users *)
let match_score u1 u2 survey = 
  let pref1 = Client.get_preferences u1 in 
  let pref2 = Client.get_preferences u2 in 
  score_aux 0. pref1 pref2 survey

let print_question s q =
  let q_list = question_list s in 
  let quest = find_q q_list q in 
  print_endline quest.q_str;
  print_string "| ";
  let rec ans = function
    | [] -> print_newline (); print_string "> ";
    | h :: t -> begin
        let inp = " [" ^ h.a_id ^ "]" in
        print_string (h.a_str ^ inp ^ " | ");
        ans t 
      end
  in ans quest.ans


(* For Testing ONLY : *)
let q1_ans0 = {
  a_id = "0";
  a_str = "I hate them!";
}

let q1_ans1 = {
  a_id = "1";
  a_str = "I do not like them";
}

let q1_ans2 = {
  a_id = "2";
  a_str = "They're useful";
}

let q1_ans3 = {
  a_id = "3";
  a_str = "I love them!";
}

let q1_ans = [q1_ans0; q1_ans1; q1_ans2; q1_ans3]

let q1_rec = {
  id = "q1";
  q_str = "How much do you like sticky notes?";
  q_type = RNG;
  ans = q1_ans
}

let q2_ans0 = {
  a_id = "0";
  a_str = "Fall";
}

let q2_ans1 = {
  a_id = "1";
  a_str = "Spring";
}

let q2_ans2 = {
  a_id = "2";
  a_str = "Winter";
}

let q2_ans3 = {
  a_id = "3";
  a_str = "Summer";
}

let q2_ans = [q2_ans0; q2_ans1; q2_ans2; q2_ans3]

let q2_rec = {
  id = "q2";
  q_str = "What is your favorite season?";
  q_type = OPT;
  ans = q2_ans
}

let q3_ans0 = {
  a_id = "0";
  a_str = "RPCC";
}

let q3_ans1 = {
  a_id = "1";
  a_str = "Appel";
}

let q3_ans = [q3_ans0; q3_ans1]

let q3_rec = {
  id = "q3";
  q_str = "RPCC or Appel";
  q_type = OPT;
  ans = q3_ans
}

let q4_ans0 = {
  a_id = "0";
  a_str = "0-2 hours";
}

let q4_ans1 = {
  a_id = "1";
  a_str = "3-5 hours";
}

let q4_ans2 = {
  a_id = "2";
  a_str = "6-10 hours";
}

let q4_ans3 = {
  a_id = "3";
  a_str = "11+ hours";
}

let q4_ans = [q4_ans0; q4_ans1; q4_ans2; q4_ans3]

let q4_rec = {
  id = "q4";
  q_str = "How many hours do you spend on social media every day?";
  q_type = RNG;
  ans = q4_ans
}

let q_list = [q1_rec; q2_rec; q3_rec; q4_rec]

let q1_type = RNG 
let q2_type = OPT
