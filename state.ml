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

let init_state () = {
  user_list = `Assoc [];
  admin_list = `Assoc [];
}

let get_data_from_file file = 
  from_file file  

let get_state ufile afile = 
  let udt = try get_data_from_file ufile with 
    | _ -> `Null in 
  let adt = try get_data_from_file afile with 
    | _ -> `Null in 
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

let add_user st store uid user =
  match st.user_list with 
  | `Assoc x -> begin 
      let users = `Assoc ((uid, user) :: x) in 
      let new_st = {st with user_list = users} in 
      if store then store_users new_st
      else new_st
    end 
  | _ -> failwith "json error"

let add_admin st store aid admin =
  match st.admin_list with 
  | `Assoc x -> begin 
      let admins = `Assoc ((aid, admin) :: x) in 
      let new_st = {st with admin_list = admins} in 
      if store then store_admins new_st 
      else new_st
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
  List.map (fun user -> (Client.get_uid user, Client.get_login user)) user_list 

let user_can_sign_up st name =
  let creds = get_user_logins st in 
  let rec check_creds credList = 
    match credList with
    | [] -> true 
    | (_, (nm, _)) :: t ->  if nm = name then false else check_creds t in 
  check_creds creds

let get_admin_logins st = 
  let aid_list = get_admins st in 
  let partial_get_admin = get_admin_by_id st in 
  let admin_list = List.map partial_get_admin aid_list in 
  List.map (fun ad -> (Admin.get_aid ad, Admin.get_login ad)) admin_list 

let admin_can_sign_up st name =
  let creds = get_admin_logins st in 
  let rec check_creds cred_list = 
    match cred_list with
    | [] -> true 
    | (_, (nm, _)) :: t ->  if nm = name then false else check_creds t in 
  check_creds creds

let validate_user st n p = 
  let cred_list = get_user_logins st in 
  let rec match_creds = function
    | [] -> raise (InvalidUser)
    | (id, (name, pword)) :: t -> begin 
        if (name, pword) = (n, p) then get_user_by_id st id 
        else match_creds t
      end in
  match_creds cred_list

let validate_admin st n p = 
  let cred_list = get_admin_logins st in 
  let rec match_creds = function
    | [] -> raise (InvalidUser)
    | (aid, (name, pword)) :: t -> begin 
        if (name, pword) = (n, p) then get_admin_by_id st aid 
        else match_creds t
      end in
  match_creds cred_list

(** [get_user_recs st] returns the list of user records *)
let get_user_recs st = 
  let assoc_list = to_assoc st.user_list in
  let usrs = snd (List.split assoc_list) in 
  List.map Client.read_json usrs 

let rec find_user_by_name name = function 
  | [] -> raise (InvalidMatch)
  | h :: t ->  begin 
      if Client.get_name h = name then h 
      else find_user_by_name name t
    end 

let rec replace_user st user =
  let replace (id, json) = 
    if Client.get_uid user = id then (Client.get_uid user, Client.to_json user) 
    else (id, json) in 
  let new_users = List.map replace (to_assoc st.user_list) in
  {st with user_list = `Assoc new_users}

let rec replace_admin st admin =
  let replace (aid, json) = 
    if Admin.get_aid admin = aid then (Admin.get_aid admin, Admin.to_json admin) 
    else (aid, json) in 
  let new_admins = List.map replace (to_assoc st.admin_list) in
  {st with admin_list = `Assoc new_admins}

let send_helper st user receiver msg = 
  Client.update_notifs receiver (Client.get_uid user) msg;
  let new_state = replace_user st receiver in 
  store_users new_state

let send_notification st user m_name msg = 
  let receiver = find_user_by_name m_name (get_user_recs st) in 
  let user_matches = Client.get_matches user |> List.split |> fst in 
  let receiver_id = Client.get_uid receiver in 
  if List.mem receiver_id user_matches then send_helper st user receiver msg
  else raise (InvalidMatch)

let add_user_to_matches st user mlist = 
  let rec aux st = function 
    | [] -> st 
    | (id, score) :: t -> begin
        let m_user = get_user_by_id st id in 
        let new_match = Client.get_uid user, score in
        let prev_matches = Client.get_matches m_user in
        Client.update_matches m_user (new_match :: prev_matches);
        let new_st = replace_user st m_user in 
        aux new_st t 
      end in 
  aux st mlist

let print_matches st user = 
  let rec print_helper = function
    | [] -> print_newline ()
    | (id, score) :: t -> begin
        let match_name = Client.get_name (get_user_by_id st id) in
        let rounded_score = 
          Float.round (score *. 100.) 
          |> int_of_float 
          |> string_of_int in  
        print_endline (match_name ^ ":" ^ "\t"  ^ rounded_score ^ "% similar"); 
        print_helper t
      end in 
  print_helper (Client.get_matches user)

let change_user_pword st user pword = 
  let new_user = Client.update_pword user pword; user in 
  let new_st = replace_user st new_user in 
  store_users new_st

let change_admin_pword st admin pword = 
  Admin.update_pword admin pword; 
  let new_st = replace_admin st admin in 
  store_admins new_st

let print_user_stats st uid = 
  let user = get_user_by_id st uid in 
  let answer_survey = Client.get_preferences user <> [] in 
  let num_matches = List.length (Client.get_matches user) in 
  let user_str = "Viewing: " ^ uid ^ " " in
  print_newline ();
  ANSITerminal.(print_string [cyan; Bold; Underlined] user_str);
  print_newline (); 
  print_newline ();
  print_endline ("Name: " ^ Client.get_name user);
  print_endline ("No. of Logins: " ^ string_of_int (Client.get_logins user));
  print_endline ("Survey Questions Answered?: " ^ string_of_bool answer_survey);
  print_endline ("Number of Matches: " ^ string_of_int num_matches);
  print_newline ();

module Flt = struct 
  include Float
  let default = 0.0
end

module Str = struct
  include String 
  let hash = Hashtbl.hash 
end

module MyGraph = Imperative.Graph.ConcreteLabeled(Str)(Flt)  

let add_vx u_name g = 
  let v = MyGraph.V.create u_name in
  MyGraph.add_vertex g v; v

let add_edges (usr, score) node g  = 
  let vx = add_vx usr g in 
  MyGraph.add_edge_e g (MyGraph.E.create node score vx )

let make_graph st = 
  let g = MyGraph.create () in 
  let name_of_id id st = id |> get_user_by_id st |> Client.get_name in
  let uname_list st = List.map (fun (id, _) -> name_of_id id st) 
      (to_assoc st.user_list) in
  let iter_helper name = 
    let match_names = 
      get_user_recs st 
      |> find_user_by_name name 
      |> Client.get_matches  
      |> List.map (fun (uid, score )-> name_of_id uid st, score) in
    let node = MyGraph.V.create name in MyGraph.add_vertex g node;
    List.iter (fun (usr, score) -> add_edges (usr, score) node g) match_names in 
  List.iter iter_helper (uname_list st); g

module GraphDraw = struct 
  include MyGraph
  let default_edge_attributes _ = []

  let get_subgraph _ = None

  let vertex_attributes _ = [`Shape `Circle] 

  let vertex_name v = MyGraph.V.label v

  let default_vertex_attributes _ = []

  let graph_attributes _ = []

  let nice_score e =  
    let flt = string_of_float (100. *. e) in 
    String.sub flt 0 (min 4 (String.length flt))  ^ "%"

  let edge_attributes e = 
    let label = MyGraph.E.label e in
    let style = if label >= 0.5 then `Bold
      else if label >= 0.25 then `Solid 
      else `Dashed in
    [`Label (nice_score (MyGraph.E.label e)) ; `Color 4711; `Arrowhead `None; 
     `Fontsize 11; `Style style ]
end

module Dot = Graph.Graphviz.Dot(GraphDraw)

module Weight = struct
  type edge = MyGraph.E.t

  type t = float

  let weight x = 1.

  let zero = 0.

  let add = (+.)

  let sub = (-.)

  let compare = compare
end

module Dij = Path.Dijkstra(MyGraph)(Weight)

let path_helper v1 v2 u1 u2 v =
  if MyGraph.V.label v = u1 then v1 := v 
  else if MyGraph.V.label v = u2 then v2 := v 
  else ()

let shortest_path st u1 u2 =
  let g = make_graph st in 
  try 
    let v1 = ref (MyGraph.V.create "") in 
    let v2 = ref (MyGraph.V.create "") in
    MyGraph.iter_vertex (fun v -> path_helper v1 v2 u1 u2 v) g;  
    let condition = MyGraph.V.label !v1 <> "" && MyGraph.V.label !v1 <> "" in
    if condition then Dij.shortest_path g !v1 !v2 |> fst |> List.length 
    else -1
  with _ -> -1

let draw_graph st = 
  let _ = Sys.command "./clear_graph.sh" in
  let file = open_out_bin "graph.dot" in
  Dot.output_graph file (make_graph st); 
  let _ = Sys.command "./make_graph.sh" in 
  ()