open Yojson.Basic.Util
open Hashtbl

type uid = string 
exception UserNotFound of uid
exception InvalidMatch 


type online_user = {
  user_id : uid;
  name : string;
  pword : string;
  mutable preferences : (string*string) list;
  mutable matches : (uid*float) list;
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

let update_matches user lst = 
  user.matches <- lst

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

let jsonify_float_assoc lst =
  `Assoc (List.map (fun (id, score) -> (id, `Float score)) lst)

let to_json user= 
  `Assoc [
    ("user_id", `String user.user_id);
    ("name", `String user.name);
    ("password", `String user.pword);
    ("preferences", jsonify_assoc user.preferences);
    ("matches", jsonify_float_assoc user.matches);
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
  let matches = json |> member "matches" |> to_assoc |>
                List.map (fun x -> match x with 
                    |(uid, `Float fl) -> (uid, fl)
                    | _-> failwith "json error") in
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

let encrypt p = p |> Hashtbl.hash |> string_of_int