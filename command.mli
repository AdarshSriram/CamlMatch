(** 
   Parsing user commands
*)

(** The type [message] represents a message that one user can send to 
    another. *)
type message = string 

(** The type [command] represents a user command that is decomposed
    into a verb and possible an object phrase. *)
type command =
  | Send of string*message
  | View of string 
  | UReset of string 
  | Quit
  | CHelp of string

(** The type [acommand] represents an admin command that is decomposed
    into a verb and possible an object phrase. *)
type acommand =
  | Hist of string
  | Graph
  | Dist of string*string
  | AReset of string
  | Quit
  | AHelp of string
  | AView of string

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse_user str] parses a user's input into a [command].
    The first word of [str] becomes the verb. The rest of the words,
    if any, become the object phrase.

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Malformed] if the command is malformed. A command
    is malformed if [str] is the empty string or contains only spaces,
    or if the verb is not a command type,
    or if the verb is "send" and first word in [str] is not a valid user,
    or if the verb is "view" and there is an empty object phrase,
    or if the verb is "reset" and there is an empty object phrase,
    or if the verb is "quit" and there is a non-empty object phrase,
    or if the verb is "help" and there is a non-empty object phrase.

*)
val parse_user : Client.t -> string -> State.state -> command

(** [parse_admin str] parses an admin's input into an [acommand].
    The first word of [str] becomes the verb. The rest of the words,
    if any, become the object phrase.

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Malformed] if the acommand is malformed. An acommand
    is malformed if [str] is the empty string or contains only spaces,
    or if the verb is not a command type,
    or if the verb is "quit" and there is a non-empty object phrase,
    or if the verb is "hist" and there is an empty object phrase,
    or if the verb is "hist" and the object phrase is not a valid Survey.qid,
    or if the verb is "dist" and the object phrase is not a pair of valid
    Client usernames,
    or if the verb is "reset" and the object phrase is not "password",
    or if the verb is "help" and the object phrase is not empty,
    or if the verb is "view" and the object phrase is not a valid Client.uid.
*)
val parse_admin : Admin.t -> string -> State.state -> acommand

(** The list of available user commands and how to use them. *)
val command_list : string

(** The list of available admin commands and how to use them. *)
val acommand_list : string
