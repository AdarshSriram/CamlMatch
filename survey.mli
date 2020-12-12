(** 
   Representation of a survey

   This module represents a survey which contains questions and their answers.
   A survey is a set-like list.
*)

(** The abstract type of values representing a survey *)
type t 
(** The abstract type representing a question in a survey *)
type q 
(** The abstract type representing a possible answer in a survey *)
type a
(** The abstract type representing a question type *)
type qtype 
type qid = string 
type aid = string


(** [from_json j] is the survey that [j] represents.
    Requires: [j] is a valid JSON survey representation. *)
val from_json : Yojson.Basic.t -> t

(** [question_list s] is the current list of questions in the survey [s]. *)
val question_list : t -> q list

(** [answer_list s id] is the list of possible answers to question [q] in 
    survey [s] 
    Raises: Failure if [id] is not valid *)
val answer_list : t -> qid -> a list  

(** [find_q qlist id] finds the question with id [id] in the question 
    list [q list] 
    Raises: Failure if [id] is not valid *)
val find_q : q list -> qid -> q 

(** [get_qid qu] returns the id of question [qu] *)
val get_qid : q -> qid 

(** [check_ans s id ans] checks if [ans] is a valid answer to the question [q] 
    in survey [s]. Returns [ans] if valid. 
    Raises: Failure if [ans] is not valid *)
val check_ans : t -> qid -> aid -> string

(** [add_question sur qu] adds [qu] to [sur] and returns a set-like list. *)
val add_question : t -> q -> t

(** [type_of_question s id] returns the qtype corresponding to the question 
    with id [id] in survey [s]
    Raises: Failure if [id] is not valid *)
val type_of_question : t -> qid -> qtype 

(** [compile_matches user state survey] returns a sorted list of matches
    for [user] with the current user list in [state] for [survey] *) 
val compile_matches : Client.t -> State.state -> q list -> 
  (Client.uid * float) list

(** [print_question s id] pretty-prints the question with id [id] from survey 
    [s] and its answers 
    Requires: [id] is a valid question id in survey [s]*)
val print_question : t -> qid -> unit

(* FOR TESTING ONLY *)
val q_list : q list 
val q3_ans : a list 
val q4_rec : q
val q1_type : qtype
val q2_type : qtype

val match_score : (qid * string) list -> (qid * string) list -> q list -> float