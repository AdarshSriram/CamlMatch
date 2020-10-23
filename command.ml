type phrase = string list

type command =
  | Chat of phrase
  | View of phrase
  | Quit

exception Empty

exception Malformed

let p el = el <> ""

(* [valid_rest command] checks if the phrase of [command]
   is valid. *)
let valid_rest command =
  failwith "Unimplemented"

let command verb rest = 
  let len = List.length rest in
  if verb = "chat" && len > 0 then valid_rest (Chat rest) else 
  if verb = "view" && len > 0 then valid_rest (View rest) else
  if verb = "quit" && len = 0 then Quit else
    raise Malformed

let parse str =
  (* failwith "Unimplemented" *)
  let lst = 
    String.split_on_char ' ' str
    |> List.filter p in
  match lst with
  | h :: t -> command h t
  | [] -> raise Empty