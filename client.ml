open Yojson.Basic.Util

type uid = string 
exception UserNotFound of uid
exception InvalidMatch 


type online_user = {
  user_id : uid;
  name : string;
  pword : string;
  mutable preferences : (string*string) list;
  mutable matches : uid list;
  mutable notifications : (uid*string) list;
}

type t = online_user 

let make_user n p id = {
  user_id = id;
  name = n;
  pword = p;
  preferences = [];
  matches = [];
  notifications = [];
}

let get_uid user = user.user_id

let get_name user  = user.name

let get_login user = (user.name, user.pword)

let get_preferences user = user.preferences

let get_matches user = user.matches

let get_notifs user = user.notifications

let update_prefs user p_list = 
  user.preferences <- p_list

let update_matches user m = 
  user.matches <- List.sort_uniq Stdlib.compare (m :: user.matches)

let update_notifs user m_id str = 
  user.notifications <- (m_id, str) :: user.notifications

let clear_notifs user = 
  user.notifications <- []

let rec user_of_uid id = function 
  | [] -> raise (UserNotFound id)
  | h :: t -> if h.user_id = id then h else user_of_uid id t  

let jsonify_list x =
  `List (List.map (fun id -> `String id) x)

let jsonify_assoc l =
  `Assoc (List.map (fun (q, ans) -> (q, `String ans)) l)

let to_json (user:online_user)= 
  `Assoc [
    ("user_id", `String user.user_id);
    ("name", `String user.name);
    ("password", `String user.pword);
    ("preferences", jsonify_assoc user.preferences);
    ("matches", jsonify_list user.matches);
    ("notifications", jsonify_assoc user.notifications);
  ] 

let read_json json =
  let id = json |> member "user_id" |> to_string in 
  let name = json |> member "name" |> to_string in 
  let pword = json |> member "password" |> to_string in 
  let prefs = json |> member "preferences" |> to_assoc |> 
              List.map (fun x -> match x with 
                  | (q, `String ans) -> (q, ans) 
                  | _ -> failwith "json error") in 
  let matches = json |> member "matches" |> to_list |>
                List.map (fun x -> match x with 
                    |`String uid -> uid | _-> failwith "json error") in
  let notifs = json |> member "notifications" |> to_assoc |> 
               List.map (fun x -> match x with 
                   | (m_id, `String msg) -> (m_id, msg) 
                   | _ -> failwith "json error") in               
  {
    user_id = id;
    name = name;
    pword = pword;
    preferences = prefs;
    matches = matches;
    notifications = notifs;
  }