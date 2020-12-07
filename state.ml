open Yojson.Basic.Util
open Yojson.Basic
open Graph

exception InvalidUser
exception InvalidMatch
exception UsernameTaken

type state = {
  user_list : Yojson.Basic.t;
  admin_list : Yojson.Basic.t;
}

let init_state () = 
  {
    user_list = `Assoc [];
    admin_list = `Assoc [];
  }

let get_data_from_file file = 
  from_file file  

let get_state ufile afile = 
  let udt = 
    try get_data_from_file ufile with 
    | _ -> `Null 
  in 
  let adt = 
    try get_data_from_file afile with 
    | _ -> `Null 
  in 
  match (udt, adt) with 
  | (`Null, `Null) -> init_state ()
  | (`Null, `Assoc x) -> {user_list = `Assoc []; admin_list = `Assoc x}
  | (`Assoc x, `Null) -> {user_list = `Assoc x; admin_list = `Assoc []}
  | (`Assoc x, `Assoc y) -> {user_list = `Assoc x; admin_list = `Assoc y}
  | _ -> failwith "Invalid file"

let get_users st = 
  st.user_list |> to_assoc |> List.map (fun (id, _) -> id) 

let get_admins st = 
  st.admin_list |> to_assoc |> List.map (fun (id, _) -> id) 

let store_users st = 
  st.user_list |> to_file "Users.json"; st

let store_admins st = 
  st.admin_list |> to_file "Admins.json"; st

let add_user st uid user =
  let admins = st.admin_list in 
  match st.user_list with 
  | `Assoc x -> begin 
      let users = `Assoc ((uid, user) :: x) in 
      store_users {user_list = users; admin_list = admins}
    end 
  | _ -> failwith "json error"

let add_admin st aid admin =
  let users = st.user_list in 
  match st.admin_list with 
  | `Assoc x -> begin 
      let admins = `Assoc ((aid, admin) :: x) in 
      store_admins {user_list = users; admin_list = admins}
    end 
  | _ -> failwith "json error"

let get_user_by_id st id =
  st.user_list |> to_assoc |> List.assoc id |> Client.read_json

let get_admin_by_id st id =
  st.admin_list |> to_assoc |> List.assoc id |> Admin.read_json

let get_user_logins st = 
  let uid_list = get_users st in 
  let partial_get_user = get_user_by_id st in 
  let user_list = List.map partial_get_user uid_list in 
  List.map (fun u -> (Client.get_uid u, Client.get_login u)) user_list 

let user_can_sign_up st name : bool =
  let creds = get_user_logins st in 
  let rec check_creds credList = 
    match credList with
    | [] -> true 
    | (_, (a, _)) ::t ->  if a = name then false else check_creds t
  in check_creds creds

let get_admin_logins st = 
  let aid_list = get_admins st in 
  let partial_get_admin = get_admin_by_id st in 
  let admin_list = List.map partial_get_admin aid_list in 
  List.map (fun a -> (Admin.get_aid a, Admin.get_login a)) admin_list 

let admin_can_sign_up st name : bool =
  let creds = get_admin_logins st in 
  let rec check_creds cred_list = 
    match cred_list with
    | [] -> true 
    | (_, (a, _)) ::t ->  if a = name then false else check_creds t
  in check_creds creds

let validate_user st n p = 
  let cred_list = get_user_logins st in 
  let rec match_creds lst = 
    match lst with 
    | [] -> raise (InvalidUser)
    | (x, (name, pword)) :: t -> begin 
        if (name, pword) = (n, p) then get_user_by_id st x 
        else match_creds t
      end
  in
  match_creds cred_list

let get_admin_logins st = 
  let aid_list = get_admins st in 
  let partial_get_admin = get_admin_by_id st in 
  let admin_list = List.map partial_get_admin aid_list in 
  List.map (fun a -> (Admin.get_aid a, Admin.get_login a)) admin_list

let validate_admin st n p = 
  let cred_list = get_admin_logins st in 
  let rec match_creds lst = 
    match lst with 
    | [] -> raise (InvalidUser)
    | (x, (name, pword)) :: t -> begin 
        if (name, pword) = (n, p) then get_admin_by_id st x 
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
  {st with user_list = `Assoc new_users}

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

let can_send st receiver user = 
  let creds = get_user_logins st in 
  let rec check_creds credList = 
    match credList with
    | [] -> false 
    | (_, (a, _)) ::t ->  if a = receiver && a <> Client.get_name user
      then true else check_creds t
  in check_creds creds

let print_user_stats st uid = 
  let user = get_user_by_id st uid in 
  let answer_survey = Client.get_preferences user <> [] in 
  let num_matches = List.length (Client.get_matches user) in 
  print_endline ("Name: " ^ Client.get_name user);
  print_endline ("No. of Logins: " ^ string_of_int (Client.get_logins user));
  print_endline ("Survey Questions Answered?: " ^ string_of_bool answer_survey);
  print_string ("Number of Matches: " ^ string_of_int num_matches)


(* FOR TESTING ONLY - REMOVE DUPLICATE CODE WITH HELPER FUNCTION *)
let test_add_user st uid user =
  let admins = st.admin_list in 
  match st.user_list with 
  | `Assoc x -> begin 
      let users = `Assoc ((uid, user) :: x) in 
      {user_list = users; admin_list = admins}
    end 
  | _ -> failwith "json error"

let test_add_admin st aid admin =
  let users = st.user_list in 
  match st.admin_list with 
  | `Assoc x -> begin 
      let admins = `Assoc ((aid, admin) :: x) in 
      {user_list = users; admin_list = admins}
    end 
  | _ -> failwith "json error"

module Flt = struct 
  type t = float

  let hash = Hashtbl.hash 

  let equal = (=)

  let compare =   Stdlib.compare

  let default = 0.0
end

module Str =
struct
  type t = string

  let hash = Hashtbl.hash 

  let equal = (=)

  let compare =   Stdlib.compare
end

module G = Imperative.Graph.ConcreteLabeled(Str)(Flt)  

module D = Graph.Traverse.Dfs(G)

module Components = Graph.Components.Make(G)

let make_graph st = 
  let g = G.create () in 
  let name_of_id id st = id |> get_user_by_id st |> Client.get_name in
  let uname_list st = to_assoc st.user_list 
                      |> List.map (fun (id, _) -> name_of_id id st ) in
  uname_list st |>
  List.iter ( fun name ->
      let match_names = 
        get_user_recs st |> find_user_by_name name
        |> Client.get_matches  
        |> List.map (fun (uid, score )-> name_of_id uid st, score) in
      let add_vx u_name = 
        let v = G.V.create u_name in G.add_vertex g v;v in
      let node = G.V.create name in G.add_vertex g node;
      List.iter (fun (usr, score) -> let vx = (add_vx usr) in 
                  G.E.create node score vx |> G.add_edge_e g) match_names;
    );
  g
(*let make_graph st = 
  let g = G.create () in 
  let id_list = to_assoc st.user_list |> List.map (fun (id, _) -> id ) in 
  id_list |>
  List.iter ( fun id ->
      let matches =  get_user_by_id st id |> Client.get_matches |> List.map (fun (uid,_ )-> int_of_string uid ) in
      let add_vx id =  let v = G.V.create id in G.add_vertex g v;v in
      let node = G.V.create (int_of_string id) in G.add_vertex g node;
      List.iter (fun usr -> let  vx = (add_vx usr) in G.add_edge g node vx; 
                  G.E.create node 1 vx |> G.add_edge_e g ) matches;);
  g*)

module Dot = Graph.Graphviz.Dot(
  struct
    include G
    let default_edge_attributes _ = []

    let get_subgraph _ = None

    let vertex_attributes _ = [`Shape `Circle] 

    let vertex_name v = G.V.label v

    let default_vertex_attributes _ = []

    let graph_attributes _ = []

    let nice_score e =  
      let flt = string_of_float (100. *. e) in 
      String.sub flt 0 (min 4 (String.length flt))  ^ "%"

    let edge_attributes e = 
      [`Label (G.E.label e |> nice_score); `Color 4711; `Arrowhead `None ]
  end
  )

let draw_graph st = 
  let file = open_out_bin "graph.dot" in 
  Dot.output_graph file (make_graph st) 