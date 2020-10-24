(* [state] stores the state of the application
   - user_list is a set-like assoc list of (user_id * Client.User objects)
   - chat_list is a set like assoc list of (chat_id * (user_id * user_id) )
     = chat_msgs is a an assoc list of ((chat_id * uid)*msg) 
*)
type state = {
  user_list: (int*Client.t) list;
  chat_list: (int * (int * int)) list;
}

exception ChatNotFound
exception  UserNotFound of int
exception UserExists of int
exception ChatExists of int

(** [init_state] is the inital state of the application*)
val init_state: unit -> state

(* [get_chats st user_id] is the chat that the user [user_id] is in.
 * -raises: ChatNotFound "No chats found" if [user_id] is not in any chat. *)
val get_chats: state -> int -> int

(* [get_users st] is the user_id list of all users.
 * -raises: UserNotFound "No online users" if the user_list is empty. *)
val get_users: state -> int list

(* [get_users_in_chat st chat_id] is the tuple of users in chat [chat_id].
 * -raises: ChatNotFound [chat_id] there is no chat with [chat_id] *)
val get_users_of_chat : state -> int -> (int*int)

(* [add_user st uid] adds user [user_id] to st.user_list
 * -raises: UserExists [user_id] if user already exists. *)
val add_user: state -> int -> Client.t -> state

(* [add_chat st id1 id2 chat] adds a new chat to  st.chat_list *)
val add_chat: state -> int -> int -> int -> state

(* [remove_user st uid] removes [uid] from st.user_list chat_list.
   -raises UserNotFound [uid] if no such user exists.
   val remove_user: state -> int -> state*)

(* [get_user_by_id st user_id] is a tuple of corresponding (user_id*CLient.User)
*)
val get_user_by_id: state -> int -> Client.t
