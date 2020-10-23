(** 
   Parsing user commands
*)

(** The type [phrase] represents the phrase that can be part of a 
    user command. Each element of the list represents a word of the phrase
    where a word is defined as a consecutive sequence of non-space
    characters. The list is in the same order as the words in the original
    player command.

    A [phrase] is not permitted to be the empty list. *)
type phrase = string list

(** The type [command] represents a user command that is decomposed
    into a verb and possible an object phrase. *)
type command =
  | Chat of phrase
  | View of phrase
  | Quit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a user's input into a [command].
    The first word of [str] becomes the verb. The rest of the words,
    if any, become the object phrase.

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is malformed if the verb is not a command type,
    or if the verb is "quit" and there is a non-empty object phrase,
    or if the verb is "chat" or "view" and there is an empty object phrase.
*)

val parse : string -> command