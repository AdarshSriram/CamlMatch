(** 
   Representation of an online user

   This module represents an online user who has a user id, a name, 
   a list of preferences, a current chat, and a list of matches. 
*)

(** The abstract type of values representing an online user *)
type t 

(** The type of user id *)
type uid = string 

(** Raised when unknown online user is encountered *)
exception UserNotFound of uid

(** [make_user n id] creates a user with an user id [id], a name [n], 
    no preferences, no current chats, and no matches 
    Requires: [id] must be unique/unused *)
val make_user : string -> uid -> t 

(** [get_uid user] returns the user id of the [user] *)
val get_uid : t -> uid 

(** [get_name user] returns the name of the [user] *)
val get_name : t -> string 

(** [get_preferences user] returns the list of preferences of the [user]. 
    Returns the empty list if [user] has no preferences*)
val get_preferences : t -> (string*string) list

(** [get_matches user] returns the list of matches of the [user]. Returns the 
    empty list if no matches are found *)
val get_matches : t -> uid list 

(** [update_prefs user p] replaces the [user's] current preferences with [p] *)
val update_prefs : t -> (string*string) list -> unit 

(** [update_matches user match] adds [match] the [user's] match list. 
    If [match] is already in the [user's] match list, then nothing is done. *)
val update_matches : t -> uid -> unit 

(** [user_of_uid id users] returns the user from [users] whose user id is [id].
    Raises: UserNotFound of [id] if [id] is not in [users] *)
val user_of_uid : uid -> t list -> t 

val to_json : t -> Yojson.Basic.t

val read_json : Yojson.Basic.t -> t

