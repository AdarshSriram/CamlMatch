type state = {
  user_list: (int*Client.t) list;
  chat_list: (int * (int * int)) list;
}

exception ChatNotFound
exception  UserNotFound of int
exception UserExists of int
exception ChatExists of int


let init_state () = 
  {
    user_list = [];
    chat_list=[]
  }

let get_chats store uid =  
  let lst = store.chat_list in
  let rec search u_id lst =
    match lst with 
    | [] -> raise ChatNotFound
    | (chat , (id1, id2)) :: t -> 
      if (id1 = u_id) || (id2=u_id) then chat else search u_id t in
  search uid lst

let get_users store = 
  List.map (fun x -> (fst x)) store.user_list

let get_users_of_chat store chat  = 
  try List.assoc chat store.chat_list with Not_found -> raise ChatNotFound 

let add_user store uid user =
  let users = if List.mem_assoc uid store.user_list then raise (UserExists uid)
    else (uid, user) :: store.user_list in 
  {store with user_list = users}

let add_chat store id1 id2 chat =
  let chats = if List.mem_assoc chat store.chat_list then raise (ChatExists chat)
    else (chat, (id1, id2)) :: store.chat_list in 
  {store with chat_list = chats}

let get_user_by_id store id =
  try List.assoc id store.user_list with Not_found -> raise (UserNotFound id)
