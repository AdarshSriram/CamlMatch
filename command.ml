type message = string 

type command =
  | Send of (string*message)
  | Quit

type acommand = 
  | Hist of string
  | Quit

exception Malformed
exception NoUserFound

let p el = el <> ""

(* [valid_send command] checks if the phrase of [command]
   is valid. *)
let valid_send st command user =
  match command with 
  | h::[] -> raise Malformed
  | h::t -> if (State.can_send st (h) user) 
    then Send (h, List.fold_left (fun acc word -> acc ^ word ^ " ") "" t) 
    else raise NoUserFound
  | [] -> raise Malformed

let command verb rest st user = 
  let len = List.length rest in
  if verb = "send" && len > 0 then valid_send st rest user 
  else if verb = "quit" && len = 0 then Quit 
  else raise Malformed

let parse_user user str st =
  let lst = 
    String.split_on_char ' ' str
    |> List.filter p in
  match lst with
  | h :: t -> command (String.lowercase_ascii h) t st user
  | [] -> raise Malformed

let valid_hist st command admin = 
  match command with 
  | [] -> raise Malformed
  | h :: t -> begin
      try
        State.question_histogram h st admin;
        Hist h
      with
      | Failure f -> raise Malformed
    end

let acommand verb rest st admin = 
  let len = List.length rest in 
  if verb = "hist" && len > 0 then valid_hist st rest admin else
  if verb = "quit" && len = 0 then Quit else 
    raise Malformed

let parse_admin admin str st = 
  let lst = 
    String.split_on_char ' ' str
    |> List.filter p in
  match lst with
  | h :: t -> acommand (String.lowercase_ascii h) t st admin
  | [] -> raise Malformed