(** [execute_system name] starts a session for user [name] *)
let execute_system name =
  failwith "Unimplemented"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "\n\nWelcome to Name of System.\n";
  print_endline "Once you fill out a profile, you will be matched with other 
  users with whom you can chat.\n";
  print_endline "Please enter your name to begin the questionaire.";
  print_string  "> ";
  match read_line () with
  | exception _ -> ()
  | name -> execute_system name

(* Execute the system engine. *)
let () = main ()