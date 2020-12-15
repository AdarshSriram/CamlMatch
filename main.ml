
(** [prompt_command user] prompts a user command for a user not in chat. *)
let rec send_notif st user uname msg =
  try  
    State.send_notification st user uname msg 
  with 
  | State.InvalidMatch -> print_endline ("You are not matched with " ^ uname); 
    st
  | _ -> print_endline "Could not send message."; st

let generate_graph st =
  State.draw_graph st; 
  print_endline "User graph generated !"

(**Helper for sign_up that safely gets user's password*)
let rec get_pwd dummy = 
  print_endline "Please enter a password (must be more than 6 characters).";
  print_string "> ";
  try let pwd = read_line () in 
    if String.length pwd < 6 then failwith "Invalid pwd" else 
      Client.encrypt pwd
  with 
  | _ -> print_endline "Invalid password"; get_pwd ()

(** [waiting_room user st] is the main system loop for a [user] in [st] *)
let rec waiting_room user st = 
  print_newline ();
  print_endline "Enter a command. Type [help] to see the list of 
  available commands.";
  print_string "> ";
  let comm = read_line () in
  try
    match Command.parse_user user comm st with 
    | Send (receiver, msg) -> 
      begin 
        let sent_state = send_notif st user receiver msg in 
        if sent_state = st then waiting_room user st 
        else print_endline "Message sent"; waiting_room user sent_state
      end
    | View _ -> waiting_room user st
    | UReset pword -> begin 
        let pword = get_pwd () in 
        waiting_room user (State.change_user_pword st user pword)
      end 
    | Quit -> ()
  with 
  | Command.Malformed -> begin 
      print_endline "\nCommand not recognized.";
      waiting_room user st
    end
  | _ -> begin 
      print_endline "\nAn error has occured.";
      waiting_room user st
    end 

let rec admin_room admin st = 
  print_newline ();
  print_endline "Enter a command. Type [help] to see the list of 
  available commands.";
  print_string "> ";
  let comm = read_line () in
  try
    match Command.parse_admin admin comm st with 
    | Hist q -> admin_room admin st
    | Graph -> State.draw_graph st; admin_room admin st 
    | Dist (u1, u2) -> 
      begin 
        let dist = State.shortest_path st u1 u2 in 
        let msg = if dist = -1 then "Invalid usernames"
          else 
            let friend = if dist = 1 then " friend " else " friends " in
            u1 ^ " is " ^ (string_of_int dist) ^  friend ^ "away from "^ u2 ^"." in 
        print_endline msg; admin_room admin st
      end 
    | AReset _ -> begin 
        let pword = get_pwd () in 
        admin_room admin (State.change_admin_pword st admin pword)
      end 
    | Quit -> ()
  with 
  | _ -> begin 
      print_endline "Command not recognized.";
      admin_room admin st
    end

(** [fill_prefs user q_list new_prefs] prompts [user] to fill out questionnaire
    [q_list] and updates prefs to [new_prefs] *)
let rec fill_prefs user q_list new_prefs survey =
  match q_list with
  | [] -> begin
      print_endline "Thank you for completing the survey!";
      Client.update_prefs user (List.rev new_prefs)
    end
  | h :: t -> begin
      let qid = Survey.get_qid h in
      Survey.print_question survey (qid);
      try 
        let ans = Survey.check_ans survey qid (read_line ()) in
        fill_prefs user t ((qid, (ans)) :: new_prefs) survey
      with
        Failure _ -> begin
          print_endline "Invalid entry";
          fill_prefs user q_list new_prefs survey
        end
    end

let display_matches user st = 
  print_endline "Here are the matches we have found: ";
  State.print_matches st user;
  waiting_room user st

let no_matches user st = 
  print_endline "Sorry, you currently do not have any matches."; 
  waiting_room user st

(** Calculates the matches for the user *)
let calc_matches user st surv = 
  let matches = Survey.compile_matches user st (Survey.question_list surv) in 
  Client.update_matches user matches;
  let updated_state = State.replace_user st user in 
  let matched_state = State.store_users updated_state in 
  if matches <> [] then display_matches user matched_state
  else no_matches user matched_state


let rec user_sign_up st survey = 
  print_endline "Please enter your name to begin the questionaire (No spaces).";
  print_string  "> ";
  try let name = read_line () in
    if String.contains name ' ' then failwith "Remove all spaces." 
    else 
    if String.length name < 1 then failwith "Name too short" 
    else 
    if not (State.user_can_sign_up st name) then raise State.UsernameTaken 
    else
      let pwd = get_pwd () in
      let user = Client.make_user name pwd 
          (State.get_users st |> List.length |> string_of_int) in 
      fill_prefs user (Survey.question_list survey) 
        (Client.get_preferences user) survey;
      let new_user_state = State.add_user st (Client.get_uid user) 
          (Client.to_json user) 
      in 
      print_endline "Please wait while we find your matches.";
      calc_matches user new_user_state survey 
  with
  | State.UsernameTaken -> 
    print_endline "Username taken."; 
    user_sign_up st survey
  | _ -> print_endline "An error has occured"; user_sign_up st survey

let rec admin_sign_up st = 
  print_endline "Please enter a name.";
  print_string "> ";
  try let name = read_line () in 
    if String.length name < 1 
    then failwith "Name too short"
    else
    if not (State.admin_can_sign_up st name) then raise State.UsernameTaken 
    else
      let pwd = get_pwd () in 
      let aid = (State.get_admins st |> List.length |> string_of_int) in
      let admin = Admin.make_admin aid name pwd in 
      let new_state = State.add_admin st aid (Admin.to_json admin) in 
      admin_room admin new_state 
  with
  | State.UsernameTaken -> 
    print_endline "Username taken."; 
    admin_sign_up st
  | _ -> print_endline "An error has occured"; admin_sign_up st 

(** [sign_up st] collects profile input to create a new user profile *)
let rec sign_up st survey =
  print_endline "Create Admin account?";
  print_endline "| Yes [0] | No [1] |";
  print_string "> ";
  try
    match read_int () with 
    | 0 -> admin_sign_up st 
    | 1 -> user_sign_up st survey 
    | _ -> print_endline "Invalid Entry"; sign_up st survey
  with 
  | _ -> print_endline "Invalid Entry"; sign_up st survey

let rec print_notifs state = function 
  | [] -> ()
  | (uid, msg) :: t -> begin 
      let sender = State.get_user_by_id state uid in 
      let name = Client.get_name sender in 
      print_endline (name ^ ":");
      print_endline msg;
      print_notifs state t
    end

let rec check_notifs user st = 
  let notifs = Client.get_notifs user in 
  if notifs <> [] then 
    begin 
      print_endline "Would you like to read your notifications?";
      print_endline "| Yes [0] | No [1] |";
      print_string "> ";
      try 
        match read_int () with 
        | 0 -> print_notifs st notifs; waiting_room user st
        | 1 -> waiting_room user st
        | _ -> failwith ""
      with 
      | e -> print_endline "Invalid Entry"; check_notifs user st
    end
  else waiting_room user st

let user_logged_in user st = 
  Client.incr_logins user;
  let logged_st = State.replace_user st user in 
  check_notifs user (State.store_users logged_st)
(* if send_notif st user = st then waiting_room user st 
   else print_endline "\nMessage sent."; waiting_room user st *)

let admin_logged_in adm st = 
  admin_room adm st

let val_admin_or_user st nm pass = 
  try 
    let user = State.validate_user st nm pass in
    user_logged_in user st
  with 
  | State.InvalidUser -> begin
      let admin = State.validate_admin st nm pass in 
      admin_logged_in admin st
    end

let rec log_in st =
  print_endline "Please enter your name";
  print_string "> ";
  try let name = read_line () in 
    print_endline "Please enter your password";
    print_string "> ";
    let pass = Client.encrypt (read_line ()) in
    val_admin_or_user st name pass 
  with
  | State.InvalidUser -> 
    print_endline "This name and password combination was not found."; log_in st
  | e -> 
    print_endline "Invalid input."; raise e

(** [execute_system dummy] starts a session for a user *)
let rec execute_system dummy =
  print_string  "> ";
  try
    let start = read_int () in
    let init_state = State.get_state "Users.json" "Admins.json" in 
    let survey = Yojson.Basic.from_file "survey1.json" |> Survey.from_json in 
    if start = 0 then sign_up init_state survey
    else if start = 1 then log_in init_state 
    else failwith "Invalid entry"
  with
  | e -> print_endline "Invalid entry"; raise e (*execute_system () *)

(** [main ()] prompts for the user to create a profile or log in, 
    then starts it. *)
let main () =
  ANSITerminal.(print_string  [red] "\n\nWelcome to Name of System.\n");
  print_endline "Sign up or Log in to be matched with other 
  users with whom you can chat.\nSign Up [0] | Log In [1]";
  ignore (execute_system ()); ()

(* Execute the system engine. *)
let () = main ()