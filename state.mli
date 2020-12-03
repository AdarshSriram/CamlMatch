(* [state] stores the state of the application
   - user_list is a set-like assoc list of (user_id * Client.User objects)
   - chat_list is a set like assoc list of (chat_id * (user_id * user_id) )
     = chat_msgs is a an assoc list of ((chat_id * uid)*msg) 
*)
type state = {
  user_list: Yojson.Basic.t;
}
exception InvalidUser
exception InvalidMatch
exception UsernameTaken

(** [init_state] is the inital state of the application*)
val init_state: unit -> state

(** [get_state file] returns the state of the system based on [file]*)
val get_state: string -> state

(* [get_users st] is the user_id list of all users.
 * -raises: UserNotFound "No online users" if the user_list is empty. *)
val get_users: state -> Client.uid list

val store_users : state -> state

(* [add_user st uid user] adds user with id [user_id] to [st] 
 * Raises: UserExists [user_id] if user already exists. *)
val add_user: state -> Client.uid -> Yojson.Basic.t -> state

(** [get_user_by_id st user_id] is a tuple of corresponding (user_id*CLient.User)
*)
val get_user_by_id : state -> Client.uid -> Client.t

val get_user_data : string -> Yojson.Basic.t

val print_users : state -> unit

(** [can_sign_up state username] is a boolean, indicating whether a user with 
    username [username] can sign up*)
val can_sign_up : state -> string -> bool

(** [can_send state uname user] is a boolean, indicating whether user [user] can
    successfully send a message to user with username [uname] *)
val can_send : state -> string -> Client.t -> bool

(** [validate_user st name pword] returns the user if the [name, pword] 
    combination is valid in state [st]. 
    Raises: [InvalidUser] if the combination is not valid *)
val validate_user : state -> string -> string -> Client.t 

(** [replace_user user st] replaces a user in [st] with its 
    updated form, [user] *)
val replace_user : state -> Client.t -> state 

(** [send_notification st user m_name msg] sends [msg] from [user] to the match 
    with name [m_name] 
    Raises: [InvalidMatch] if [m_name] is not a match of [user] or 
    if [m_name] is not an existing user *)
val send_notification : state -> Client.t -> string -> string -> state 

(** [read_notifs st user] pretty-prints the [user]'s notfications to the 
    console *)
val read_notifs : state -> Client.t -> unit

(** [print_matches st user] pretty-prints [user's] matches to the console in 
    the sytem's state [state] *)
val print_matches: state -> Client.t -> unit 

(* FOR TESTING ONLY *)
val testing_store_users : state -> state

(* [add_user st uid user] adds user with id [user_id] to [st] 
 * Raises: UserExists [user_id] if user already exists. *)
val testing_add_user: state -> Client.uid -> Yojson.Basic.t -> state


val test_state : state
val empty_state : state
val pref_state : state

val u1 : Yojson.Basic.t
val u2 : Yojson.Basic.t
val u3 : Yojson.Basic.t
val u4 : Yojson.Basic.t
val u5 : Yojson.Basic.t
val u6 : Client.t

val pref_1 : Yojson.Basic.t
val pref_2 : Yojson.Basic.t
val pref_3 : Yojson.Basic.t
val pref_4 : Yojson.Basic.t


