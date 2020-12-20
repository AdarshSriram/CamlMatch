(** 
   Representation of an online user

   This module represents an online user who has a user id, a name, the number
   of times logged in, a list of preferences, a list of notifications, 
   and a list of matches. 
*)

(** The abstract type representing a user *)
type t 

(** The type of user id *)
type uid = string 

(** Raised when unknown user is encountered *)
exception UserNotFound of uid

(** [make_user nm pw id] creates a user with an user id [id], a name [nm], 
    an ecrypted password [pw], no log ins, no preferences, no notifications, 
    and no matches 
    Requires: [id] must be unique/unused, [nm] must not have any spaces, 
    [pw] must be at least 6 characters *)
val make_user : string -> string -> uid -> t 

(** [get_uid user] returns the user id of the [user]
    Requires: [user] must be a valid representation of a user *)
val get_uid : t -> uid 

(** [get_name user] returns the name of the [user] 
    Requires: [user] must be a valid representation of a user*)
val get_name : t -> string 

(** [get_login user] returns the user name and password of [user]
    Requires: [user] must be a valid representation of a user *)
val get_login : t -> string*string

(** [get_preferences user] returns the list of preferences of the [user]. 
    Returns the empty list if [user] has no preferences
    Requires: [user] must be a valid representation of a user *)
val get_preferences : t -> (string*string) list

(** [get_notifs user] returns the [user]'s notficiations. 
    Returns the empty list if [user] has no notfications 
    Requires: [user] must be a valid representation of a user *)
val get_notifs : t -> (uid*string) list

(** [get_matches user] returns the list of matches of the [user]. Returns the 
    empty list if the user has no matches
    Requires: [user] must be a valid representation of a user *)
val get_matches : t -> (uid*float) list 

(** [get_logins user] returns the number of times [user] has logged in to the 
    system 
    Requires: [user] must be a valid representation of a user *)
val get_logins : t -> int 

(** [update_prefs user pref_list] replaces the [user]'s current preferences 
    with [pref_list] 
    Requires: [user] must be a valid representation of a user *)
val update_prefs : t -> (string*string) list -> unit 

(** [update_matches user match] adds [match] the [user]'s match list. 
    If [match] is already in the [user]'s match list, then nothing is done. 
    Requires: [user] must be a valid representation of a user *)
val update_matches : t -> (uid*float) list -> unit 

(** [update_notifs user m_id msg] adds the [msg] from [m_id] to [user]'s 
    notfication list. 
    Requires: [user] must be a valid representation of a user *)
val update_notifs : t -> uid -> string -> unit 

(** [clear_notifs user] removes all of [user]'s notifications 
    Requires: [user] must be a valid representation of a user *)
val clear_notifs : t -> unit 

(** [incr_logins user] increments the number of times [user] has logged in
    by 1 
    Requires: [user] must be a valid representation of a user *)
val incr_logins : t -> unit

(** [update_pword user pw] updates the [user]'s password  to the encrypted 
    string [pw]
    Requires: [user] must be a valid representation of  auser *)
val update_pword : t -> string -> unit

(** [user_of_uid id users] returns the user from [users] whose user id is [id].
    Raises: UserNotFound of [id] if [id] is not in [users] *)
val user_of_uid : uid -> t list -> t 

(** [to_json user] converts [user] into a json format 
    Requires: [user] must be a valid representation of a user*)
val to_json : t -> Yojson.Basic.t

(** [read_json j] converts [j] into a user 
    Requires: [j] must be a valid json representation of a user *)
val read_json : Yojson.Basic.t -> t

(** [encrypt p] converts [p] to its encrypted form *)
val encrypt : string -> string
