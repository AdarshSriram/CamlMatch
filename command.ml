type message = string 

type command =
  | Send of (string*message)
  | View 
  | Quit

exception Malformed
exception NoUserFound

let p el = el <> ""

(* [valid_rest command] checks if the phrase of [command]
   is valid. *)
let valid_rest st command user =
  match command with 
  | h::[] -> raise Malformed
  | h::t -> if (State.can_send st (h) user) 
    then Send (h, List.fold_left (fun acc word -> acc ^ word ^ " ") "" t) 
    else raise NoUserFound
  | [] -> raise Malformed

let command verb rest st user = 
  let len = List.length rest in
  if verb = "send" && len > 0 then valid_rest st rest user else 
  if verb = "view" && len = 0 then View else
  if verb = "quit" && len = 0 then Quit else
    raise Malformed

let parse user str st =
  let lst = 
    String.split_on_char ' ' str
    |> List.filter p 
  in
  match lst with
  | h :: t -> command (String.lowercase_ascii h) t st user
  | [] -> raise Malformed