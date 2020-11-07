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