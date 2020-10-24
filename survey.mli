(** 
   Representation of a survey

   This module represents a survey which contains questions and their answers.
   A survey is a set-like list.
*)

(** The abstract type of values representing a question *)
type t 

(** [empty] creates an empty survey. *)
val empty : t list

(** [check_ans q ans] checks if [ans] is a valid answer to [q] *)
val check_ans : t -> string -> int

(** [add_question sur qu] adds [qu] to [sur] and returns a set-like list. *)
val add_question : t list -> t -> t list

(** [rem_question q sur] removes the question with "question" field [q]
    from [sur]. *)
val rem_question : string -> t list -> t list

(** [print_question q] pretty-prints [q] and its answers *)
val print_question : t -> unit

(** [question_list] is the current list of questions in the survey. *)
val question_list : t list

(** Survey questions for testing *)
val q1 : t
val q2 : t
val q3 : t
val q4 : t