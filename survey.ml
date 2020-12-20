open Yojson.Basic.Util

type qid = string
type aid = string

type qtype = | RNG | OPT

type t = Yojson.Basic.t 

type a = {
  a_id : aid;
  a_str : string
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
let string_of_json s name = s |> member name |> to_string

(** [list_of_json s name] converts the value associated with key [name] 
    in the JSON object [s] into a list
    Requires: [s] is a valid JSON representation of a survey *)
let list_of_json s name = s |> member name |> to_list

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
  q_type = qtype_of_string (string_of_json q "qtype" );
  ans = List.map to_answer_type (list_of_json q "answers")
}

let question_list s = List.map to_question_type (list_of_json s "questions")

(** [find_ans_to_q q qlist] returns the answers to the question in [qlist] 
    with id [q]
    Raises: Failure if [q] is not a valid id *)
let rec find_ans_to_q q = function 
  | [] -> failwith "Invalid question id"
  | h :: t -> if h.id = q then h.ans else find_ans_to_q q t

let answer_list s q = find_ans_to_q q (question_list s) 

(** [get_q q qlist] returns the question from [qlist] with id [q]
    Raises: Failure if [q] is not a valid id *)
let rec get_q qid = function 
  | [] -> failwith "Invalid question id"
  | h :: t -> if h.id = qid then h else get_q qid t 

let find_q qlist qid = get_q qid qlist 

let get_qid q = q.id

let check_ans s qid ans =
  let a_list = answer_list s qid in
  let aid_list = List.map (fun a -> a.a_id) a_list in 
  if List.mem ans aid_list then ans
  else failwith "Invalid entry"

let type_of_question s qid = 
  let q_list = question_list s in 
  let quest = get_q qid q_list in 
  quest.q_type

let score_rng a1 a2 q = 
  let div = float_of_int (List.length q.ans) -. 1. in 
  1. -. (Float.abs (float_of_string a1 +. float_of_string a2) /. div)

let score_opt a1 a2 = 
  if a1 = a2 then 1. else 0.

let score_aux_helper qid1 qid2 a1 a2 survey score = 
  if qid1 <> qid2 then failwith "Preference list not in order" 
  else begin
    let q = get_q qid1 survey in  
    if q.q_type = RNG then score +. score_rng a1 a2 q
    else score +. score_opt a1 a2 
  end

(* This iterates through preference lists of both users and calls
   score_rng or score_opt to calculare the total score *)
let rec score_aux score p1 p2 survey = 
  match p1 with 
  | [] -> score 
  | (qid1, a1) :: t1 -> begin 
      match p2 with 
      | [] -> failwith "Incomplete preferences list"
      | (qid2, a2) :: t2 -> begin 
          let acc = score_aux_helper qid1 qid2 a1 a2 survey score in
          score_aux acc t1 t2 survey
        end
    end

(* calculates match score of 2 users *)
let match_score p1 p2 survey = 
  let total = score_aux 0. p1 p2 survey in 
  total /. (float_of_int (List.length p1))

let rec compile_helper state user_prefs acc ulst survey = 
  match ulst with 
  | [] -> acc 
  | h :: t -> begin 
      let u = State.get_user_by_id state h in
      let match_prefs = Client.get_preferences u in
      let new_acc = (h, match_score user_prefs match_prefs survey) :: acc in 
      compile_helper state user_prefs new_acc t survey 
    end

let comp_scores (_, score1) (_, score2) = 
  if score1 > score2 then -1
  else if score1 < score2 then 1
  else 0

(** [find_above_mean lst] returns members of [lst] with a score greater than 
    the mean score *)
let find_above_mean lst = 
  let add_score acc (_, score) = acc +. score in 
  let mean = (List.fold_left add_score 0. lst) /. 
             float_of_int (List.length lst) in 
  if mean = 0. then [] 
  else List.filter (fun (_, score) -> score >= mean) lst  

let compile_matches user state survey = 
  let q_list = question_list survey in 
  let users = State.get_users state in 
  let other_users = List.filter (fun x -> x <> Client.get_uid user) users in
  let user_prefs = Client.get_preferences user in
  let all_sim_scores = compile_helper state user_prefs [] other_users q_list in
  let match_list = find_above_mean all_sim_scores in  
  List.sort comp_scores match_list

let print_question s qid =
  let q_list = question_list s in 
  let quest = find_q q_list qid in 
  print_endline quest.q_str;
  print_string "| ";
  let rec ans = function
    | [] -> print_newline (); print_string "> ";
    | h :: t -> begin
        let inp = " [" ^ h.a_id ^ "]" in
        print_string (h.a_str ^ inp ^ " | ");
        ans t 
      end in 
  ans quest.ans

let rec add_ans ans acc = 
  match ans, acc with 
  | 0, h :: t -> (h + 1) :: t
  | n, h :: t -> h :: add_ans (n - 1) t 
  | n, [] -> add_ans n (0 :: acc)

let count_ans qid prefs acc = 
  try
    let ans = int_of_string (List.assoc qid prefs) in 
    add_ans ans acc
  with 
  | Not_found -> failwith "Question not found."

let rec get_prefs st qid acc survey = function 
  | [] -> List.rev acc
  | h :: t -> begin 
      let user_pref = h |> State.get_user_by_id st |> Client.get_preferences in
      let new_acc = count_ans qid user_pref acc in 
      get_prefs st qid new_acc survey t
    end

let rec add_bar lst acc = 
  match lst with 
  | [] -> acc 
  | h :: t -> begin
      if h = 0 then add_bar t acc ^ "__ "
      else add_bar t acc ^ "   "
    end

let rec add_qnum n str c = 
  if c > n then str
  else begin 
    let str = str ^ (string_of_int c) ^ "  " in 
    add_qnum n str (c + 1)
  end

let display_line lst count = 
  let last_line = 
    let len = List.length lst in
    let str = "1 |" ^ String.make (1 + 3 * len) '_' in 
    add_qnum len (str ^ "\n    ") 1 in
  if count = 0 then last_line
  else begin
    let str = (string_of_int (count + 1)) ^ " | " in 
    add_bar lst str
  end

let rec print_ansi lst = function 
  | -1 -> () 
  | n -> begin 
      ANSITerminal.(print_string [cyan; Bold] (display_line lst n));
      print_newline ();
      print_ansi (List.map (fun x -> x - 1) lst) (n - 1)
    end

let display_histogram st qid survey = 
  let users = State.get_users st in
  let answers = answer_list survey qid in
  let init_acc = List.init (List.length answers) (fun _ -> 0) in
  let hist = get_prefs st qid init_acc survey users in 
  let user_len = List.length users in 
  let rev_hist = List.map (fun x -> user_len - x) hist in 
  print_newline ();
  ANSITerminal.(print_string [Bold;Underlined] ("Question: " ^ qid));
  print_newline ();
  print_newline ();
  print_ansi rev_hist user_len

let question_histogram qid st survey = 
  display_histogram st qid survey

(* For Testing ONLY : *)
let test_hist_values st qid users survey = 
  let answers = answer_list survey qid in
  let init_acc = List.init (List.length answers) (fun _ -> 0) in
  get_prefs st qid init_acc survey users

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

let get_q3_ans () = [q3_ans0; q3_ans1]

let q3_rec = {
  id = "q3";
  q_str = "RPCC or Appel";
  q_type = OPT;
  ans = get_q3_ans ()
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

let get_q4_rec () = {
  id = "q4";
  q_str = "How many hours do you spend on social media every day?";
  q_type = RNG;
  ans = q4_ans
}

let get_q_list () = [q1_rec; q2_rec; q3_rec; get_q4_rec ()]

let get_q1_type () = RNG 
let get_q2_type () = OPT
