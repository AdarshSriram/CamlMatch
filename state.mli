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

(* [add_user st uid] adds user [user_id] to st.user_list
 * -raises: UserExists [user_id] if user already exists. *)
val add_user: state -> Client.uid -> Yojson.Basic.t -> state

(** [get_user_by_id st user_id] is a tuple of corresponding (user_id*CLient.User)
*)
val get_user_by_id : state -> Client.uid -> Client.t

val store_users : state -> state

val get_user_data : string -> Yojson.Basic.t

val print_users : state -> unit

val can_sign_up : state -> string -> bool

(** [validate_user st name pword] returns the user if the [name, pword] 
    combination is valid in state [st]. 
    Raises: [InvalidUser] if the combination is not valid *)
val validate_user : state -> string -> string -> Client.t 

(** [send_notification st user m_name msg] sends [msg] from [user] to the match 
    with name [m_name] 
    Raises: [InvalidMatch] if [m_name] is not a match of [user] or 
    if [m_name] is not an existing user *)
val send_notification : state -> Client.t -> string -> string -> state 

(** [read_notifs user] pretty-prints the [user]'s notfications to the 
    console *)
val read_notifs : state -> Client.t -> unit

(* FOR TESTING ONLY *)
val test_state : state

val u1 : Yojson.Basic.t
val u2 : Yojson.Basic.t
val u3 : Yojson.Basic.t
val u4 : Yojson.Basic.t
val u5 : Yojson.Basic.t
val u6 : Client.t

