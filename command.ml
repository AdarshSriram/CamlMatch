type message = string 

type command =
  | Send of string * message
  | View of string
  | UReset of string
  | Quit
  | CHelp of string

type acommand = 
  | Hist of string
  | Graph
  | Dist of string * string
  | AReset of string
  | Quit
  | AHelp of string
  | AView of string

exception Malformed

let not_empty el = el <> ""

let command_list = 
  "\n\t[send uname msg] sends [msg] to the user with username [uname]
  \t[view matches] prints your match list
  \t[reset password] allows you to reset your password
  \t[quit] exits you out of the system"

let acommand_list = 
  "\n\t[hist qid] displays a histogram describing how users answered the 
  \t\tquestion with ID [qid]. (ex: \"hist q1\")
  \t[graph] creates a connection graph visualizing the network of matches
  \t[dist u1 u2] returns how many users [u1] and [u2] are from each other in terms
  \t\tof matches, where [u1], [u2] are usernames
  \t[view uid] displays data about user with ID [uid]
  \t[reset password] allows you to reset your password
  \t[quit] exits you out of the system"

let fold_message msg = 
  List.fold_left (fun acc word -> acc ^ word ^ " ") "" msg

(** [valid_send command] checks if the phrase of [command]
    is valid. *)
let valid_send st command user =
  match command with 
  | h :: [] -> raise Malformed
  | h :: t -> Send (h, fold_message t) 
  | [] -> raise Malformed

(** [valid_view st rest user] checks if [rest] is valid and calls the 
    appropriate function based on [rest]. 
    Raises: Malformed if [rest] is invalid *)
let valid_view st rest user = 
  match rest with 
  | h :: [] -> begin 
      if h = "matches" then (State.print_matches st user; View h)
      else raise Malformed
    end 
  | [] | _ -> raise Malformed

let valid_ureset st rest user = 
  match rest with 
  | h :: [] -> begin 
      if h = "password" then (UReset h)
      else raise Malformed
    end 
  | [] | _ -> raise Malformed

let command verb rest st user = 
  let len = List.length rest in
  if verb = "send" && len > 0 then valid_send st rest user 
  else if verb = "view" && len > 0 then valid_view st rest user
  else if verb = "reset" && len > 0 then valid_ureset st rest user
  else if verb = "quit" && len = 0 then Quit 
  else if verb = "help" && len = 0 then CHelp command_list
  else raise Malformed

let parse_user user input st =
  let lst = List.filter not_empty (String.split_on_char ' ' input) in
  match lst with
  | h :: t -> command (String.trim (String.lowercase_ascii h)) t st user
  | [] -> raise Malformed

let valid_hist st command admin = 
  match command with 
  | h :: [] -> Hist h
  | _ -> raise Malformed

let valid_dist st admin command = 
  match command with 
  | uname1 :: uname2 :: [] -> Dist (uname1, uname2)
  | _ -> raise Malformed

let valid_areset st rest user = 
  match rest with 
  | h :: [] -> begin 
      if h = "password" then (AReset h)
      else raise Malformed
    end 
  | [] | _ -> raise Malformed

let valid_aview st rest admin = 
  match rest with 
  | h :: [] -> AView h
  | _ -> raise Malformed

let acommand verb rest st admin = 
  let len = List.length rest in 
  if verb = "hist" && len > 0 then valid_hist st rest admin 
  else if verb = "graph" && len = 0 then Graph 
  else if verb = "dist" && len > 0 then valid_dist st admin rest 
  else if verb = "reset" && len > 0 then valid_areset st rest admin   
  else if verb = "quit" && len = 0 then Quit  
  else if verb = "help" && len = 0 then AHelp acommand_list 
  else if verb = "view" && len > 0 then valid_aview st rest admin
  else raise Malformed

let parse_admin admin input st = 
  let lst = List.filter not_empty ( String.split_on_char ' ' input) in
  match lst with
  | h :: t -> acommand (String.trim (String.lowercase_ascii h)) t st admin
  | [] -> raise Malformed