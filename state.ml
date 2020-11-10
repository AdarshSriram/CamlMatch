open Yojson.Basic.Util
open Yojson.Basic

exception InvalidUser
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

let u1 = Client.to_json (Client.make_user "user 1" "pass1" "1" )
let u2 = Client.to_json (Client.make_user "user 2" "pass2" "2")
let u3 = Client.make_user "user 3" "pass3" "3" |> Client.to_json
let u4 = Client.make_user "user 4" "pass4" "4" |> Client.to_json
let u5 = Client.make_user "user 5" "pass5" "5" |> Client.to_json

let u6 = 
  let user  = (Client.make_user "user 6" "pass6" "6" ) in 
  Client.update_matches user "2";
  Client.update_matches user "3";
  Client.update_prefs user [("1", "2");("2", "1")]; user 

let test_state = 
  { 
    user_list = `Assoc [("1", u1);("2", u2);("3", u3);("4", u4)];
  }