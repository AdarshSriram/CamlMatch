(** Representation of the state of the system *)

(** The abstract type that represents the state of the system *)
type state 

(** Raised when an invalid username/password combo cannot be validated *)
exception InvalidUser

(** Raised when a user attempts to send a message to someone who they are
    not matched with *)
exception InvalidMatch

(** Raised when a user attempts to sign up with a user name that is already
    in the system *)
exception UsernameTaken

(** [init_state] is the inital state of the application *)
val init_state: unit -> state

(** [get_state ufile afile] returns the state of the system based on user data 
    [ufile] and admin data [afile]. The [init_state] is returned if both files
    are empty 
    Requires: [ufile] and [afile] must be accurate json files representing 
    users and admins, respectively *)
val get_state: string -> string -> state

(** [get_users st] returns the list of all user ids in state [st]. Returns
    the empty list if there are no current users in [st] *)
val get_users: state -> Client.uid list

(** [get_admins st] returns the list of all admin ids in state [st]. Returns 
    the empty list if there are no current admins in [st] *)
val get_admins: state -> Admin.aid list

(** [store_users st] adds the users in state [st] to the Users.json file and 
    returns the new state *)
val store_users : state -> state

(** [store_admins st] adds the admins in state [st] to the Admins.json file and 
    returns the new state *)
val store_admins : state -> state

(* [add_user st store uid user] adds user with id [user_id] to [st] and 
    returns the updated state. The function calls [store_users] if store = true
    otherwise it just returns the updated state.
 * Requires: [uid] must not be taken *)
val add_user: state -> bool -> Client.uid -> Yojson.Basic.t -> state

(* [add_admin st aid admin] adds [admin] with id [a_id] to [st] and returns the 
   updated state. The function calls [store_admins] if store = true
    otherwise it just returns the updated state.
 * Requires: [aid] must not be taken *)
val add_admin: state -> bool -> Admin.aid -> Yojson.Basic.t -> state

(** [get_user_by_id st uid] returns the user associated the the id [user_id] 
    in the state [st] 
    Requires: There must be a user assocated with id [uid] in the state [st] *)
val get_user_by_id : state -> Client.uid -> Client.t

(** [get_admin_by_id st aid] returns the admin associated the the id 
    [a_id] in the state [st] 
    Requires: There must be an admin assocated with id [aid] in the state [st] 
*)
val get_admin_by_id : state -> Admin.aid -> Admin.t

(** [user_can_sign_up st name] returns true if no other user in the state [st] 
    has the user name [name]. Returns false otherwise *)
val user_can_sign_up : state -> string -> bool

(** [admin_can_sign_up st name] returns true if no other admin in the state [st] 
    has the name [name]. Returns false otherwise *)
val admin_can_sign_up : state -> string -> bool

(** [validate_user st name pword] returns the user in the state [st] who is
    associated with [name, pword] combination.  
    Raises: [InvalidUser] if no user associates with the [name, pword] 
    combination in state [st] *)
val validate_user : state -> string -> string -> Client.t 

(** [validate_admin st name pword] returns the admin in the state [st] who is
    associated with [name, pword] combination.  
    Raises: [InvalidUser] if no user associates with the [name, pword] 
    combination in state [st] *)
val validate_admin : state -> string -> string -> Admin.t

(** [replace_user user st] replaces the user in [st] with its 
    updated form, [user]
    Requires: [user] must have the same user id as the user it is trying to
    replace *)
val replace_user : state -> Client.t -> state 

(** [send_notification st user m_name msg] sends [msg] from [user] to the match 
    with name [m_name] 
    Raises: [InvalidMatch] if [m_name] is not a match of [user] or 
    if [m_name] is not the name of an existing user *)
val send_notification : state -> Client.t -> string -> string -> state 

(** [add_user_to_matches st user mlist] adds the [user] as a match to all the
    users whose names appear in the list [mlist] and returns the updated state
*)
val add_user_to_matches : state -> Client.t -> (Client.uid * float) list 
  -> state

(** [change_user_pword st user pword] updates the [user]'s password to the 
    encrypted form for [pword] and returns the new state with the updated 
    user *)
val change_user_pword : state -> Client.t -> string -> state 

(** [change_admin_pword st admin pword] updates the [admin]'s password to the 
    encrypted form for [pword] and returns the new state with the updated 
    admin *)
val change_admin_pword : state -> Admin.t -> string -> state 

(** [print_matches st user] pretty-prints the [user]'s matches to the console in 
    the sytem's state [state] *)
val print_matches : state -> Client.t -> unit  

(** [draw_graph state] creates a graph representation of [state]'s user network
    as a .dot file, and creates a visualation of the graph as a PDF, with 
    usernames as nodes and similarity indices of a pair of matched users as 
    edge labels. *)
val draw_graph : state -> unit

(** [shortest_path state user1 user2] is the number of friends apart users with 
    usenames user1 and user2 are. Returns -1 if u1 and u2 are not part of the 
    same connected component or either one is not a valid username *)
val shortest_path : state -> string -> string -> int

(** [print_user_stats st uid] prints information about the user with uid [uid]. 
    Information includes username, number of times logged in, information about
    the whether they survey was filled out, and the number of matches 
    Requires: [uid] must be a valid user id in state [st] *)
val print_user_stats : state -> Client.uid -> unit

(** [get_user_recs st] returns the list of users that are in state [st]. 
    Returns the empty list if there are no users in [st] *)
val get_user_recs : state -> Client.t list
