type t = {
  question : string;
  answers : string list;
}

let same_q q1 q2 =
  if q1.question = q2.question &&
     let rec same_ans b y1 y2= 
       match (y1, y2) with
       | ([],[]) -> b
       | (h :: t, a :: b) -> same_ans (h = a) t b
       | ([],_) | (_,[]) -> false
     in 
     same_ans false q1.answers q2.answers
  then 0 else 1

let add_question lst q =
  List.sort_uniq same_q (q :: lst)

let rec rem_question q lst = 
  match lst with
  | [] -> []
  | h :: t -> begin
      if h.question = q then t
      else h :: (rem_question q t)
    end

let post_q = {
  question = "Do you like post-it notes?";
  answers = ["yes";"no"]
}

let question_list = [
  post_q
]

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
