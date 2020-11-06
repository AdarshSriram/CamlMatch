(** [prompt_command user] prompts a user command for a user not in chat. *)
let rec prompt_command user =
  failwith "Unimplemented"

(** [waiting_room st] tells user to wait while they are matched to other users
    in state [st] *)
let waiting_room st = 
  print_endline "Please wait while we calculate your matches."

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

(** [sign_up st] collects profile input to create a new user profile *)
let rec sign_up st survey =
  print_endline "Please enter your name to begin the questionaire.";
  print_string  "> ";
  try let name = read_line () in
    if String.length name < 1 then failwith "Invalid Entry" else
      let user = Client.make_user name ( State.get_users st |> List.length |> string_of_int) in 
      fill_prefs user (Survey.question_list survey) 
        (Client.get_preferences user) survey;
      let new_user_state = State.add_user st (Client.get_uid user) (Client.to_json user) in 
      waiting_room new_user_state
  with
  | _ -> print_endline "Invalid entry"; sign_up st survey

let log_in x =
  failwith "Unimplemented"

(** [execute_system dummy] starts a session for a user *)
let rec execute_system dummy =
  print_string  "> ";
  try
    let start = read_int () in
    let init_state = State.init_state () in 
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