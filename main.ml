(** [prompt_command user] prompts a user command for a user not in chat. *)
let rec prompt_command user =
  failwith "Unimplemented"

(** [waiting_room st] tells user to wait while they are matched to other users
    in state [st] *)
let waiting_room st = 
  print_endline "Please wait while we calculate your matches."

(** [fill_prefs user q_list new_prefs] prompts [user] to fill out questionnaire
    [q_list] and updates prefs to [new_prefs] *)
let rec fill_prefs user q_list new_prefs =
  match q_list with
  | [] -> begin
      print_endline "Thank you for completing the survey!";
      Client.update_prefs user (List.rev new_prefs)
    end
  | h :: t -> begin
      Survey.print_question h;
      try fill_prefs user t (Survey.check_ans h (read_line ()) :: new_prefs)
      with
        Failure _ -> begin
          print_endline "Invalid entry";
          fill_prefs user q_list new_prefs
        end
    end

(** [sign_up st] collects profile input to create a new user profile *)
let sign_up st =
  print_endline "Please enter your name to begin the questionaire.";
  print_string  "> ";
  let name =
    match read_line () with
    | exception _ -> failwith "TODO"
    | nm -> nm
  in
  let user = Client.make_user name (List.length (State.get_users st)) in 
  fill_prefs user Survey.question_list (Client.get_preferences user);
  let new_user_state = State.add_user st (Client.get_uid user) user in 
  waiting_room new_user_state

let log_in x =
  failwith "Unimplemented"

(** [execute_system name] starts a session for user [name] *)
let execute_system input =
  let init_state = State.init_state () in 
  if input = 0 then sign_up init_state 
  else if input = 1 then log_in init_state 
  else failwith "Invalid input"

(** [main ()] prompts for the user to create a profile or log in, 
    then starts it. *)
let main () =
  ANSITerminal.(print_string  [red] "\n\nWelcome to Name of System.\n");
  print_endline "Sign up or Log in to be matched with other 
  users with whom you can chat.\nSign Up [0] | Log In [1]";
  print_string  "> ";
  match read_line () with
  | exception _ -> ()
  | x -> execute_system (int_of_string x)

(* Execute the system engine. *)
let () = main ()