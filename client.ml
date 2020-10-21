type uid = int 
type curr_chat = int*uid
exception UserNotFound of uid

type online_user = {
  user_id : uid;
  name : string;
  mutable preferences : int list;
  mutable matches : online_user list;
  mutable current_chat : curr_chat option;
}

type t = online_user 

let make_user n id = {
  user_id = id;
  name = n;
  preferences = [];
  matches = [];
  current_chat = None;
}

let get_uid user = user.user_id

let get_name user = user.name

let get_preferences user = user.preferences

let get_matches user = user.matches

let get_current_chat user = user.current_chat

let update_prefs user p_list = 
  user.preferences <- p_list

let update_matches user m = 
  user.matches <- List.sort_uniq Stdlib.compare (m :: user.matches)

let update_current_chat user chat_id user2 = 
  user.current_chat <- Some (chat_id, user2.user_id);
  user2.current_chat <- Some (chat_id, user.user_id)

let rec user_of_uid id = function 
  | [] -> raise (UserNotFound id)
  | h :: t -> if h.user_id = id then h else user_of_uid id t
