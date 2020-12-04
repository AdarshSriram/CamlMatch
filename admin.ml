open Yojson.Basic.Util

type aid = string 

type t = {
  a_id : aid;
  name : string;
  pword : string;
}

let make_admin id n p =
  {
    a_id = id;
    name = n;
    pword = p;
  }

let get_aid ad = ad.a_id

let get_name ad = ad.name

let get_login ad = (ad.name, ad.pword)


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

let encrypt p = p |> Hashtbl.hash |> string_of_int

