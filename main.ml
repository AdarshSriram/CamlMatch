(** Prompts a user command for a user not in chat. *)
let rec prompt_command user =
  failwith "Unimplemented"

(** Prompts user to fill out questionnaire and updates prefs *)
let rec fill_prefs user q_list new_prefs =
  match q_list with
  | [] -> begin
      print_endline "Thank you for completing the survey!";
      Client.update_prefs user (List.rev new_prefs)
    end
  | h :: t -> begin
      Survey.print_question h;
      try fill_prefs user t (int_of_string (read_line ()) :: new_prefs)
      with
        Failure _ -> begin
          print_endline "Invalid entry";
          fill_prefs user q_list new_prefs
        end
    end

(** Collects profile input to create a new user profile *)
let sign_up x =
  (* failwith "Unimplemented" *)
  print_endline "Please enter your name to begin the questionaire.";
  print_string  "> ";
  let name =
    match read_line () with
    | exception _ -> failwith "TODO"
    | nm -> nm
  in
  let user = Client.make_user name 0 in 
  fill_prefs user Survey.question_list (Client.get_preferences user)

let log_in x =
  failwith "Unimplemented"

(** [execute_system name] starts a session for user [name] *)
let execute_system input =
  if input = 0 then sign_up 0 else if input = 1 then log_in 0 else failwith "j"

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