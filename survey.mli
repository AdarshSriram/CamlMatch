(** 
   Representation of a survey

   This module represents a survey which contains questions and their answer 
   choices.

   A valid survey contains each of the following data for each question:
   - question id
   - string represenation of the question
   - a question type
   - a list of answers where each answer contains an answer id and a string
        representation of the answer
*)

(** The abstract type representing a survey *)
type t 

(** The abstract type representing a question in a survey *)
type q 

(** The abstract type representing a possible answer for a quesiton *)
type a

(** The abstract type representing a type of question *)
type qtype 

(** The id for a question *)
type qid = string 

(** The id for an answer *)
type aid = string

(** [from_json file] is the survey that the json [file] represents.
    Requires: [file] is a valid JSON survey representation. *)
val from_json : Yojson.Basic.t -> t

(** [question_list s] is the current list of questions in the survey [s].
    Requires: [s] must be valid survey *)
val question_list : t -> q list

(** [answer_list s qid] is the list of possible answers to question [qid] in 
    survey [s] 
    Requires: [s] must be a valid survey
    Raises: Failure when [qid] is not a valid question id in [s] *)
val answer_list : t -> qid -> a list  

(** [find_q qlist qid] finds the question with id [qid] in the question 
    list [q list] 
    Requires: [qid] must be a valid question id in [qlist] *)
val find_q : q list -> qid -> q 

(** [get_qid q] returns the id of question [q] *)
val get_qid : q -> qid 

(** [check_ans s qid ans] checks if [ans] is a valid answer to the question 
    [qid] in survey [s]. Returns [ans] if valid. 
    Raises: Failure if [ans] is not valid *)
val check_ans : t -> qid -> aid -> string

(** [type_of_question s qid] returns the qtype corresponding to the question 
    with id [qid] in survey [s]
    Raises: Failure if [qid] is not valid *)
val type_of_question : t -> qid -> qtype 

(** [compile_matches user state s] returns a sorted list of matches
    for [user] with the current user list in [state] for [s]
    Requires: [user] must be a valid user, [state] must be a valid state, 
    and [s] must be a valid survey *) 
val compile_matches : Client.t -> State.state -> t -> (Client.uid * float) list

(** [print_question s qid] pretty-prints the question with id [qid] from survey 
    [s] and its answers 
    Requires: [qid] is a valid question id in survey [s]*)
val print_question : t -> qid -> unit

(** [question_histogram qid st admin s] displays a histogram of user
    responses for question [qid] in [st] and [s].
    Raises: Failure if [qid] is not a question id in survey [s] *)
val question_histogram : qid -> State.state -> t -> unit

(** FOR TESTING ONLY *)
val test_hist_values : State.state -> qid -> Client.uid list -> t -> int list

(** FOR TESTING ONLY *)
val get_q_list : unit ->  q list 

(** FOR TESTING ONLY *)
val get_q3_ans : unit ->  a list 

(** FOR TESTING ONLY *)
val get_q4_rec : unit -> q

(** FOR TESTING ONLY *)
val get_q1_type : unit ->  qtype

(** FOR TESTING ONLY *)
val get_q2_type : unit ->  qtype

(** FOR TESTING ONLY *)
val match_score : (qid * string) list -> (qid * string) list -> q list -> float