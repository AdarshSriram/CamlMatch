type state = {
  user_list: (int * Client.t) list;
  chat_list: (int * (int * int)) list;
}

exception ChatNotFound
exception UserNotFound of int
exception UserExists of int
exception ChatExists of int


let init_state () = 
  {
    user_list = [];
    chat_list = []
  }

let get_chats st uid =  
  let lst = st.chat_list in
  let rec search u_id lst =
    match lst with 
    | [] -> raise ChatNotFound
    | (chat , (id1, id2)) :: t -> begin 
        if (id1 = u_id) || (id2 = u_id) then chat 
        else search u_id t 
      end
  in
  search uid lst

let get_users st = 
  List.map (fun x -> (fst x)) st.user_list

let get_users_of_chat st chat  = 
  try List.assoc chat st.chat_list 
  with Not_found -> raise ChatNotFound 

let add_user st uid user =
  let users = 
    if List.mem_assoc uid st.user_list 
    then raise (UserExists uid)
    else (uid, user) :: st.user_list 
  in 
  {st with user_list = users}

let add_chat st id1 id2 chat =
  let chats = 
    if List.mem_assoc chat st.chat_list 
    then raise (ChatExists chat)
    else (chat, (id1, id2)) :: st.chat_list
  in 
  {st with chat_list = chats}

let get_user_by_id st id =
  try List.assoc id st.user_list 
  with Not_found -> raise (UserNotFound id)
