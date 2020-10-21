(** 
   Representation of a survey

   This module represents a survey which contains questions and their answers.
   A survey is a set-like list.
*)

(** The abstract type of values representing a question *)
type t 

(** [add_question sur qu] adds [qu] to [sur] and returns a set-like list. *)
val add_question : t list -> t -> t list

(** [rem_question q sur] removes the question with "question" field [q]
    from [sur]. *)
val rem_question : string -> t list -> t list

(** [question_list] is the current list of questions in the survey. *)
val question_list : t list

(** [print_question q] pretty-prints [q] and its answers *)
val print_question : t -> unit