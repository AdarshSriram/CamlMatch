let current_survey = "survey1.json"

let rec send_notif st user uname msg =
  try State.send_notification st user uname msg with 
  | State.InvalidMatch -> 
    print_endline ("You are not matched with " ^ uname); st
  | _ -> print_endline "Could not send message."; st

(**Helper for sign_up that safely gets user's password*)
let rec get_pwd () = 
  print_endline "Please enter a password (must be more than 6 characters).";
  print_string "> ";
  try 
    let pwd = read_line () in 
    if String.length pwd < 6 then failwith "Invalid pwd" 
    else Client.encrypt pwd
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
    | Send (receiver, msg) -> exec_send st user receiver msg
    | View _ -> waiting_room user st
    | UReset pword -> exec_reset st user
    | Quit -> exit 0
    | CHelp txt -> print_endline txt; waiting_room user st
  with 
  | Command.Malformed ->  
    print_endline "\nCommand not recognized."; waiting_room user st
  | _ -> print_endline "\nAn error has occured."; waiting_room user st

and exec_send st user receiver msg = 
  let sent_state = send_notif st user receiver msg in 
  if sent_state = st then waiting_room user st 
  else print_endline "Message sent"; waiting_room user sent_state

and exec_reset st user = 
  let pword = get_pwd () in 
  waiting_room user (State.change_user_pword st user pword)

let rec admin_room admin st = 
  print_newline ();
  print_endline "Enter a command. Type [help] to see the list of 
  available commands.";
  print_string "> ";
  let comm = read_line () in
  try
    match Command.parse_admin admin comm st with 
    | Hist q -> exec_hist st admin q
    | Graph -> exec_graph st admin 
    | Dist (u1, u2) -> exec_dist st admin u1 u2
    | AReset _ -> exec_admin_reset st admin
    | Quit -> exit 0
    | AHelp txt -> print_endline txt; admin_room admin st
    | AView user -> exec_view st admin user
  with 
  | _ -> print_endline "Command not recognized."; admin_room admin st

and exec_hist st admin q = 
  let s = Survey.from_json (Yojson.Basic.from_file current_survey) in
  Survey.question_histogram q st s;
  admin_room admin st

and exec_graph st admin = 
  State.draw_graph st; 
  print_endline "UserGraph.pdf created"; 
  admin_room admin st 

and exec_dist st admin u1 u2 = 
  let dist = State.shortest_path st u1 u2 in 
  let friend = if dist = 2 then " friend " else " friends " in
  let msg = 
    if dist = -1 then "Invalid usernames"
    else begin
      u1 ^ " is " ^ (string_of_int (dist - 1)) ^  friend ^ "away from " 
      ^ u2 ^ "." 
    end in 
  print_newline ();
  print_endline msg; admin_room admin st

and exec_admin_reset st admin = 
  let pword = get_pwd () in 
  admin_room admin (State.change_admin_pword st admin pword)

and exec_view st admin user = 
  if List.mem user (State.get_users st) 
  then (State.print_user_stats st user; admin_room admin st)
  else print_endline "User not found."; admin_room admin st

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
      | Failure _ -> begin
          print_endline "Invalid entry.";
          fill_prefs user q_list new_prefs survey
        end
      | _ -> begin 
          print_endline "Something went wrong, please try again."; 
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
  let matches = Survey.compile_matches user st surv in 
  Client.update_matches user matches;
  let updated_state = State.replace_user st user in 
  let matched_state = State.store_users updated_state in 
  let add_matches_st = State.add_user_to_matches matched_state user matches in 
  let final_state = State.store_users add_matches_st in 
  if matches <> [] then display_matches user final_state
  else no_matches user final_state

let create_user name st survey = 
  let pwd = get_pwd () in
  let user = Client.make_user name pwd 
      (State.get_users st |> List.length |> string_of_int) in 
  let q_list = Survey.question_list survey in 
  let user_prefs = Client.get_preferences user in
  fill_prefs user q_list user_prefs survey;
  let new_user_state = State.add_user st true (Client.get_uid user) 
      (Client.to_json user) in 
  print_endline "Please wait while we find your matches.";
  calc_matches user new_user_state survey 

let rec user_sign_up st survey = 
  print_endline "Please enter your name to begin the questionaire (No spaces).";
  print_string  "> ";
  try 
    let name = read_line () in
    if String.contains name ' ' then failwith "Remove all spaces." 
    else if String.length name < 1 then failwith "Name too short." 
    else if not (State.user_can_sign_up st name) then raise State.UsernameTaken 
    else create_user name st survey
  with
  | State.UsernameTaken -> 
    print_endline "Username taken."; user_sign_up st survey
  | _ -> print_endline "An error has occured"; user_sign_up st survey

let create_admin name st = 
  let pwd = get_pwd () in 
  let aid = (State.get_admins st |> List.length |> string_of_int) in
  let admin = Admin.make_admin aid name pwd in 
  let new_state = State.add_admin st true aid (Admin.to_json admin) in 
  admin_room admin new_state 

let rec admin_sign_up st = 
  print_endline "Please enter a name.";
  print_string "> ";
  try let name = read_line () in 
    if String.length name < 1 
    then failwith "Name too short"
    else if not (State.admin_can_sign_up st name) then raise State.UsernameTaken
    else create_admin name st
  with
  | State.UsernameTaken -> print_endline "Username taken."; admin_sign_up st
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
    | _ -> print_endline "Invalid Entry."; sign_up st survey
  with 
  | _ -> print_endline "Invalid Entry."; sign_up st survey

let rec print_notifs state = function 
  | [] -> ()
  | (uid, msg) :: t -> begin 
      let sender = State.get_user_by_id state uid in 
      let name = Client.get_name sender in 
      print_endline (name ^ ": " ^ msg);
      print_notifs state t
    end

let rec check_notifs user st = 
  let notifs = Client.get_notifs user in 
  if notifs <> [] then process_notifs st user notifs
  else waiting_room user st

and process_notifs st user notifs = 
  print_endline "Would you like to read your notifications?";
  print_endline "| Yes [0] | No [1] |";
  print_string "> ";
  try 
    match read_int () with 
    | 0 -> print_notifs st notifs; waiting_room user st
    | 1 -> waiting_room user st
    | _ -> failwith ""
  with 
  | e -> print_endline "Invalid Entry."; check_notifs user st

let user_logged_in user st = 
  Client.incr_logins user;
  let logged_st = State.replace_user st user in 
  check_notifs user (State.store_users logged_st)

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
  try 
    let name = read_line () in 
    print_endline "Please enter your password";
    print_string "> ";
    let pass = Client.encrypt (read_line ()) in
    val_admin_or_user st name pass 
  with
  | State.InvalidUser -> 
    print_endline "This name and password combination was not found."; 
    log_in st
  | e -> print_endline "Invalid Entry."; log_in st

(** [execute_system dummy] starts a session for a user *)
let rec execute_system () =
  print_string  "> ";
  try
    let init_state = State.get_state "Users.json" "Admins.json" in 
    let survey = Survey.from_json (Yojson.Basic.from_file current_survey) in 
    let start = read_int () in 
    if start = 0 then sign_up init_state survey
    else if start = 1 then log_in init_state 
    else print_endline "Invalid Entry."; execute_system ()
  with
  | e -> print_endline "Invalid Entry."; execute_system ()

(** [main ()] prompts for the user to create a profile or log in, 
    then starts it. *)
let main () =
  ANSITerminal.(print_string  [red] "\n\nWelcome to CamlMatch.\n");
  print_endline "Sign up or Log in to be matched with other 
  users with whom you can chat.\nSign Up [0] | Log In [1]";
  ignore (execute_system ()); ()

(* Execute the system engine. *)
let () = main ()