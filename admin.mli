(** 
   Representation of an administator

   This module represents an administrate who has an admin id, a name, 
   and a password
*)

(** The abstract type representing an admin *)
type t 

(** The type of admin id *)
type aid = string 

(** [make_admin id name pword] creates an admin with admin id [id], 
    name [name], and password [pword]*)
val make_admin : aid -> string -> string -> t

(** [get_aid ad] returns the id of the admin [ad] *)
val get_aid : t -> aid 

(** [get_name ad] returns the username of the admin [ad] *)
val get_name : t -> string 

(** [get_login ad] returns the username/password combo for the admin [ad] *)
val get_login : t -> string * string

(** [update_pword admin p] updates the [admins's] password [p], which is 
    encrypted *)
val update_pword : t -> string -> unit 

(** [to_json ad] converts [ad] into a json format *)
val to_json : t -> Yojson.Basic.t

(** [read_json j] converts [j] into an admin *)
val read_json : Yojson.Basic.t -> t

(** [encrypt p] converts [p] to its encrypted form *)
val encrypt : string -> string
