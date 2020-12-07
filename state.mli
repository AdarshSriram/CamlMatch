(** Representation of the state of the system *)

(** The abstract type that represents the state of the system *)
type state 
exception InvalidUser
exception InvalidMatch
exception UsernameTaken

(** [init_state] is the inital state of the application*)
val init_state: unit -> state

(** [get_state ufile afile] returns the state of the system based on user data 
    [ufile] and admin data [afile]*)
val get_state: string -> string -> state

(* [get_users st] returns the user_id list of all users in state [st]. 
 * Raises: UserNotFound "No online users" if the user_list is empty. *)
val get_users: state -> Client.uid list

(** [get_admins st] returns the admin_id list of all admins in the state [st]*)
val get_admins: state -> Admin.aid list

(** [store_users st] adds the users in state [st] to the storage file and 
    returns the new state *)
val store_users : state -> state

(** [store_admins st] adds the admins in state [st] to the storage file and 
    returns the new state *)
val store_admins : state -> state

(* [add_user st uid user] adds user with id [user_id] to [st] and returns the 
   updated state
 * Requires: [uid] must not be taken *)
val add_user: state -> Client.uid -> Yojson.Basic.t -> state

(* [add_admin st aid admin] adds [admin] with id [a_id] to [st] and returns the 
   updated state
 * Requires: [uid] must not be taken *)
val add_admin: state -> Admin.aid -> Yojson.Basic.t -> state

(** [get_user_by_id st user_id] returns the user associated the the id [user_id] 
    in the state [st] *)
val get_user_by_id : state -> Client.uid -> Client.t

(** [get_admin_by_id st a_id] returns the admin associated the the id 
    [a_id] in the state [st] *)
val get_admin_by_id : state -> Admin.aid -> Admin.t

(** [user_can_sign_up st name] returns true if no other user in the state [st] 
    has the user name [name]. Returns false otherwise *)
val user_can_sign_up : state -> string -> bool

(** [can_send state uname user] is a boolean, indicating whether user [user] can
    successfully send a message to user with username [uname] *)
val can_send : state -> string -> Client.t -> bool
(** [admin_can_sign_up st name] returns true if no other admin in the state [st] 
    has the name [name]. Returns false otherwise *)
val admin_can_sign_up : state -> string -> bool

(** [can_send state uname user] is a boolean, indicating whether user [user] can
    successfully send a message to user with username [uname] *)
val can_send : state -> string -> Client.t -> bool

(** [validate_user st name pword] returns the user if the [name, pword] 
    combination is valid in state [st]. 
    Raises: [InvalidUser] if the combination is not valid *)
val validate_user : state -> string -> string -> Client.t 

(** [validate_admin st name pword] returns the admin if the [name, pword] 
    combination is valid in state [st]. 
    Raises: [InvalidUser] if the combination is not valid *)
val validate_admin : state -> string -> string -> Admin.t

(** [replace_user user st] replaces the user in [st] with its 
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
val print_matches : state -> Client.t -> unit  

(** [get_user_recs st] returns the users in state [st] *)
val get_user_recs : state -> Client.t list 

(** [print_user_stats st uid] prints information about the user with uid [uid]. 
    Information includes username, number of times logged in, preferences, and 
    number of matches *)
val print_user_stats : state -> Client.uid -> unit

(** For testing *)
val test_add_user : state -> Client.uid -> Yojson.Basic.t -> state

val test_add_admin : state -> Admin.aid -> Yojson.Basic.t -> state
