(** 
   Representation of an administator

   This module represents an administrator who has an admin id, a name, 
   and a password
*)

(** The abstract type representing an admin *)
type t 

(** The type of admin id *)
type aid = string 

(** [make_admin id nm pw] creates an admin with admin id [id], 
    name [nm], and password [pw]
    Requires: [nm] and [pw] are not empty strings. [id] is not taken *)
val make_admin : aid -> string -> string -> t

(** [get_aid admin] returns the id of the admin [admin] 
    Requires: [admin] is a valid representation of an admin *)
val get_aid : t -> aid 

(** [get_name admin] returns the username of the admin [admin] 
    Requires: [admin] is a valid representation of an admin *)
val get_name : t -> string 

(** [get_login admin] returns the username/password combo for the admin [admin] 
    Requires: [admin] is a valid representation of an admin*)
val get_login : t -> string * string

(** [update_pword admin pw] updates the [admin]'s password with the encrypted 
    string [pw]
    Requires: [admin] is a valid representation of an admin. 
    [p] is not an empty string *)
val update_pword : t -> string -> unit 

(** [to_json admin] converts [admin] into a json format 
    Requires: [admin] is a valid representation of an admin *)
val to_json : t -> Yojson.Basic.t

(** [read_json j] converts [j] into an admin representation 
    Requires: [j] is a valid json representation of an admin *)
val read_json : Yojson.Basic.t -> t

(** [encrypt pw] converts [pw] to its encrypted form *)
val encrypt : string -> string
