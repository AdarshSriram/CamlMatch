
(** [prompt_command user] prompts a user command for a user not in chat. *)
let rec prompt_command user =
  failwith "Unimplemented"

(** [waiting_room st] tells user to wait while they are matched to other users
    in state [st] *)
let waiting_room user st = 
  ()

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

(**Helper for sign_up that safely gets user's password*)
let rec get_pwd dummy = 
  print_endline "Please enter a password (must be more than 6 characters).";
  print_string "> ";
  try let pwd = read_line () in 
    if String.length pwd < 6 then failwith "Invalid pwd" else 
      pwd
  with 
  | _ -> print_endline "Invalid password"; get_pwd ()

(** Calculates the matches for the user *)
let calc_matches user st surv = 
  let matches = Survey.compile_matches user st (Survey.question_list surv) in 
  Client.update_matches user matches;
  let updated_state = State.replace_user st user in 
  let matched_state = State.store_users updated_state in 
  print_endline "Here are the matches we have found: ";
  State.print_matches matched_state user;
  waiting_room user matched_state

(** [sign_up st] collects profile input to create a new user profile *)
let rec sign_up st survey =
  print_endline "Please enter your name to begin the questionaire.";
  print_string  "> ";
  try let name = read_line () in
    if String.length name < 1 then failwith "Invalid Entry" else
    if not (State.can_sign_up st name) then raise State.UsernameTaken else
      let pwd = get_pwd () in
      let user = Client.make_user name pwd 
          ( State.get_users st |> List.length |> string_of_int) in 
      fill_prefs user (Survey.question_list survey) 
        (Client.get_preferences user) survey;
      let new_user_state = State.add_user st (Client.get_uid user) 
          (Client.to_json user) 
      in 
      print_endline "Please wait while we calculate your matches.";
      print_endline "Please wait while we find your matches.";
      calc_matches user new_user_state survey 
  with
  | State.UsernameTaken -> print_endline "Username taken."; sign_up st survey
  | _ -> print_endline "An error has occured"; sign_up st survey

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
  print_endline "Would you like to read your notifications?";
  print_endline "| Yes [0] | No [1] |";
  try 
    let ans = read_int () in 
    if ans = 0 then print_notifs st notifs else ();
    waiting_room user st 
  with 
  | _ -> print_endline "Invalid Entry"; check_notifs user st


let rec log_in st =
  print_endline "Please enter your name";
  print_string "> ";
  try let name = read_line () in 
    print_endline "Please enter your password";
    print_string "> ";
    let pass = read_line () in
    let user = State.validate_user st name pass in 
    check_notifs user st;
    waiting_room user st
  with
  | State.InvalidUser -> 
    print_endline "This name and password combination was not found."; log_in st
  | _ -> 
    print_endline "Invalid input."; log_in st

(** [execute_system dummy] starts a session for a user *)
let rec execute_system dummy =
  print_string  "> ";
  try
    let start = read_int () in
    let init_state = State.get_state "Users.json" in 
    let survey = Yojson.Basic.from_file "survey1.json" |> Survey.from_json in 
    if start = 0 then sign_up init_state survey
    else if start = 1 then log_in init_state 
    else failwith "Invalid entry"
  with
  | _ -> print_endline "Invalid entry"; execute_system ()

(** [main ()] prompts for the user to create a profile or log in, 
    then starts it. *)
let main () =
  ANSITerminal.(print_string  [red] "\n\nWelcome to Name of System.\n");
  print_endline "Sign up or Log in to be matched with other 
  users with whom you can chat.\nSign Up [0] | Log In [1]";
  ignore (execute_system ()); ()

(* Execute the system engine. *)
let () = main ()