open Yojson.Basic.Util

type aid = string 

type t = {
  a_id : aid;
  name : string;
  mutable pword : string;
}

let make_admin id nm pw = {
  a_id = id;
  name = nm;
  pword = pw;
}

let get_aid admin = admin.a_id

let get_name admin = admin.name

let get_login admin = (admin.name, admin.pword)

let encrypt pw = pw |> Hashtbl.hash |> string_of_int

let update_pword admin pw = admin.pword <- pw

let to_json admin = 
  `Assoc [
    ("admin_id", `String admin.a_id);
    ("name", `String admin.name);
    ("password", `String admin.pword);
  ] 

let read_json json =
  let id = json |> member "admin_id" |> to_string in 
  let name = json |> member "name" |> to_string in 
  let pword = json |> member "password" |> to_string in 
  {
    a_id = id;
    name = name;
    pword = pword;
  }

