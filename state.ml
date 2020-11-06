open Yojson.Basic.Util
open Yojson.Basic

type state = {
  user_list: Yojson.Basic.t;
}

let init_state () = 
  {
    user_list = `Assoc [];
  }

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
  st.user_list|>to_assoc |> List.assoc id |> Client.read_json

let get_user_data file = 
  from_file file 

let print_users st =
  st.user_list |> to_string |> print_string