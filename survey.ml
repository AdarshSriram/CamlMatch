(* TODO: make survey a json file *)

type t = {
  question : string;
  answers : string list;
}

let empty = []

let check_ans q ans =
  let a = int_of_string ans in
  if a >= 0 && a < List.length q.answers then a
  else failwith "Invalid entry"

let same_q q1 q2 =
  if q1.question = q2.question &&
     let rec same_ans b y1 y2= 
       match (y1, y2) with
       | ([],[]) -> b
       | (h :: t, a :: b) -> same_ans (h = a) t b
       | ([],_) | (_,[]) -> false
     in 
     same_ans true q1.answers q2.answers
  then 0 else -1 (* -1 is returned to preserve list order *)

let add_question lst q =
  List.sort_uniq same_q (q :: lst)

let rec rem_question q lst = 
  List.filter (fun h -> h.question <> q) lst

let print_question q =
  print_endline q.question;
  print_string "| ";
  let rec ans ind = function
    | [] -> print_newline (); print_string "> ";
    | h :: t -> begin
        let inp = " [" ^ (string_of_int ind) ^ "]" in
        print_string (h ^ inp ^ " | ");
        ans (ind + 1) t 
      end
  in ans 0 q.answers

let q1 = {
  question = "Do you like post-it notes?";
  answers = ["yes";"no"]
}
let q2 = {
  question = "What is your favorite season?";
  answers = ["fall";"spring";"winter";"summer"]
}
let q3 = {
  question = "Do you use pens or pencils?";
  answers = ["pen";"pencil"]
}
let q4 = {
  question = "RPCC or Appel?";
  answers = ["RPCC";"Appel"]
}

let question_list = [
  q1;
  q2;
  q3;
  q4;
]