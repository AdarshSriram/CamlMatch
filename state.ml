open Yojson.Basic.Util
open Yojson.Basic

exception InvalidUser
exception InvalidMatch
exception UsernameTaken

type state = {
  user_list: Yojson.Basic.t;
}

let init_state () = 
  {
    user_list = `Assoc [];
  }

let get_user_data file = 
  from_file file 

let get_state file = 
  let dt = 
    try get_user_data file with 
    | _ -> `Null 
  in 
  match dt with 
  | `Null -> init_state ()
  | `Assoc x -> { user_list = `Assoc x}
  | _ -> failwith "Invalid file"

let get_users st = 
  st.user_list |> to_assoc |> List.map (fun (id, _) -> id) 

let store_users st = 
  st.user_list |> to_file "Users.json"; st

let add_user st uid user =
  match st.user_list with 
  | `Assoc x -> let users = `Assoc ((uid, user)::x) in 
    store_users { user_list = users}
  | _ -> failwith "json error"

let get_user_by_id st id =
  st.user_list |> to_assoc |> List.assoc id |> Client.read_json

let print_users st =
  st.user_list |> to_string |> print_string

let get_logins st = 
  let uid_list = get_users st in 
  let partial_get_user = get_user_by_id st in 
  let user_list = List.map partial_get_user uid_list in 
  List.map (fun u -> (Client.get_uid u, Client.get_login u)) user_list 

let validate_user st n p = 
  let cred_list = get_logins st in 
  let rec match_creds lst = 
    match lst with 
    | [] -> raise (InvalidUser)
    | (x, (name, pword)) :: t -> begin 
        if (name, pword) = (n, p) then get_user_by_id st x 
        else match_creds t
      end
  in
  match_creds cred_list

(** [get_user_recs st] returns the list of user records *)
let get_user_recs st = 
  let assoc_list = st.user_list |> to_assoc in
  let usrs = List.split assoc_list |> snd in 
  List.map Client.read_json usrs 

let rec find_user_by_name name = function 
  | [] -> raise (InvalidMatch)
  | h :: t ->  begin 
      if Client.get_name h = name then h 
      else find_user_by_name name t
    end 

let rec replace_user st user =
  let repl (x, y) = 
    if Client.get_uid user = x 
    then (Client.get_uid user, Client.to_json user) 
    else (x, y) 
  in 
  let new_users = List.map repl (to_assoc st.user_list) in
  {user_list = `Assoc new_users}

let send_notification st user (m_name : string) msg = 
  let receiver = get_user_recs st |> find_user_by_name m_name in 
  let user_matches = Client.get_matches user |> List.split |> fst in 
  if not (List.mem (Client.get_uid receiver) user_matches)
  then raise (InvalidMatch)
  else begin 
    Client.update_notifs receiver (Client.get_uid user) msg;
    let new_state = replace_user st receiver in 
    store_users new_state
  end 

let read_notifs st user = 
  let rec print_to_console notifs = 
    match notifs with
    | [] -> Client.clear_notifs user; print_newline ()
    | (x, y) :: t -> begin 
        let match_name = get_user_by_id st x |> Client.get_name in 
        print_endline (match_name ^ ":" ^"\t"  ^ y);
        print_to_console t
      end 
  in
  print_to_console (Client.get_notifs user)


let print_matches st user = 
  let rec print_helper = function
    | [] -> print_newline ()
    | (id, score) :: t -> begin
        let match_name = get_user_by_id st id |> Client.get_name in
        let rounded_score =  Float.round (score *. 100.) 
                             |> int_of_float 
                             |> string_of_int in  
        print_endline (match_name ^ ":" ^"\t"  ^ rounded_score ^ "% similar"); 
        print_helper t
      end
  in 
  print_helper (Client.get_matches user)

let can_sign_up st name : bool =
  let creds = get_logins st in 
  let rec check_creds credList = 
    match credList with
    | [] -> true 
    | (_, (a, _)) ::t ->  if a = name then false else check_creds t
  in check_creds creds

let can_send st receiver user = 
  let creds = get_logins st in 
  let rec check_creds credList = 
    match credList with
    | [] -> false 
    | (_, (a, _)) ::t ->  if a = receiver && a <> Client.get_name user
      then true else check_creds t
  in check_creds creds

(*FOR TESTING ONLY *)

let testing_store_users st = 
  st.user_list |> to_file "DummyUsers.json"; st

let testing_add_user st uid user =
  match st.user_list with 
  | `Assoc x -> let users = `Assoc ((uid, user)::x) in 
    testing_store_users { user_list = users}
  | _ -> failwith "json error"


let u1 = Client.to_json (Client.make_user "user 1" "pass1" "1") 
let u2 = Client.to_json (Client.make_user "user 2" "pass2" "2")
let u3 = Client.make_user "user 3" "pass3" "3" |> Client.to_json
let u4 = Client.make_user "user 4" "pass4" "4" |> Client.to_json
let u5 = Client.make_user "user 5" "pass5" "5" |> Client.to_json

let u6 = 
  let user  = (Client.make_user "user 6" "pass6" "6" ) in 
  Client.update_matches user [("2", 0.543); ("3", 0.998)];
  Client.update_prefs user [("1", "2");("2", "1")]; user 

let test_state = 
  { 
    user_list = `Assoc [("1", u1);("2", u2);("3", u3);("4", u4)];
  }

let empty_state = 
  { 
    user_list = `Assoc [("1", u1);];
  }

let create_pref_user uname pword id prefs = 
  let u = Client.make_user uname pword id in
  Client.update_prefs u prefs; u

let pref_1 = create_pref_user "p1" "" "1"  
    [("q1", "0"); ("q2", "0"); ("q3", "0"); ("q4", "0")] |> Client.to_json

let pref_2 = create_pref_user "p2" "" "2"
    [("q1", "1"); ("q2", "1"); ("q3", "0"); ("q4", "1")] |> Client.to_json

let pref_3 = create_pref_user "p3" "" "3" 
    [("q1", "3"); ("q2", "1"); ("q3", "1"); ("q4", "2")] |> Client.to_json

let pref_4 = create_pref_user "p4" "" "4" 
    [("q1", "3"); ("q2", "1"); ("q3", "1"); ("q4", "3")] |> Client.to_json

let pref_state = {
  user_list = `Assoc [("1", pref_1);("2", pref_2);("3", pref_3);("4", pref_4)];
}

