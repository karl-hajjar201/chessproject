(* Note: You may introduce new code anywhere in this file. *) 
open Yojson.Basic.Util

type item_id = string
type room_id = string
type exit_name = string
type item = {
  item_id : item_id;
  score : int;
  target : room_id;
  location : room_id
}

(** Type that helps make exits of rooms accessable to functions*)
type exit = {
  name : exit_name;
  where_to : room_id;
}

exception UnknownRoom of room_id
exception UnknownExit of exit_name
exception UnknownItem of item_id

(* TODO: replace [unit] with a type of your own design. *)
(** Type that helps make rooms accessable*)
type room = {
  id : room_id;
  description : string;
  exits : exit list;
  score : int;
}

type t = {
  rooms : room list;
  start_room : room_id;
  target_room : room_id;
  items : item list;
}
(** [exit_of_j j] creates a exit of type [exit] from json[j]*)
let exit_of_j j = {
  name = j |> member "name" |> to_string;
  where_to = j |> member "room id" |> to_string;
}

let item_of_j j = {
  item_id = j |> member "id" |> to_string;
  score = j |> member "score" |> to_int;
  target = j |> member "target" |> to_string;
  location = j |> member "location" |> to_string
}


(** [room_of_j j] creates a room of type [room] from json[j] *)
let room_of_j j = {
  id = j |> member "id" |> to_string;
  score = j |> member "score" |> to_int;
  description = j |> member "description" |> to_string;
  exits = j |> member "exits" |> to_list |> List.map exit_of_j;
}
(** [t_of_j j] creates an element of type t from json[j]*)
let t_of_j j = {
  start_room = j |> member "start room" |> to_string;
  target_room = j |> member "target room" |> to_string;
  rooms = j |> member "rooms" |> to_list |> List.map room_of_j;
  items = j |> member "items" |> to_list |> List.map item_of_j;
}

(** Exposed Function*)
let from_json json = 
  try t_of_j json
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

(** [make_setlike acc li] takes in [] for the acc and a generic list[li] and
    outputs a list that has doubles removed but maintains order unlike
    List.sort_uniq*)
let rec make_setlike acc li =
  match li with
  |[] -> acc
  |h::t -> if List.mem h acc then make_setlike acc t 
    else make_setlike (List.cons h acc) t


(** Exposed Function*)
let start_room adv =
  adv.start_room

let target_room adv =
  adv.target_room
let item_id  item = 
  item.item_id

(** [find_room_id room] outputs the room_id of a room[room] *)
let find_room_id room =
  room.id
(** Exposed Function*)
let room_ids adv = 
  let room_list = adv.rooms in
  let output = List.map find_room_id room_list in
  make_setlike [] output


(** [get_description room_id room_list] outputs the description of [room_id]
    Raises Unknown room if room_id is invalid*)
let rec get_description room_id room_list :string =
  match room_list with
  |[] -> raise (UnknownRoom room_id)
  |h::i -> if h.id = room_id then h.description else get_description room_id i
(** Exposed Function*)
let description adv room =
  get_description room adv.rooms


(** [get_exit_name ex] outputs the exit_name of an exit [ex] *)
let get_exit_name ex =
  ex.name
(** [get_exits room_id room_list] outputs the list of exits of a room with 
    room_id [room_id] 
    Raises Unknown room if room_id is invalid*)
let rec get_exits room_id room_list :exit list =
  match room_list with
  |[] -> raise (UnknownRoom room_id)
  |h::i -> if h.id = room_id then h.exits else get_exits room_id i
(** Exposed Function*)
let exits adv room = 
  make_setlike [] (List.map get_exit_name (get_exits room adv.rooms))


let rec score_help room_list room_id=
  match room_list with
  |[] -> raise (UnknownRoom room_id)
  |h::i -> if h.id = room_id then h.score else score_help i room_id
(** Exposed Function*)
let get_score adv room_id = 
  score_help adv.rooms room_id

let in_inventory item =
  item.location = "inventory"

let in_room room_id item=
  item.location = room_id

let item_score item=
  item.score

let rec item_id_ls = function
  |[] -> []
  |h::t -> h.item_id :: (item_id_ls t)


(** [get_room rooms room_id]  *)
let rec get_room rooms room_id =
  match rooms with
  |[]-> raise (UnknownRoom room_id)
  |h::t -> if h.id = room_id then h else get_room t room_id
(** [get_exit_room adv exits_li exit_name] returns the room of that 
    exit [exit_name] leads to
    Raises Unknown exit if exit doesn't exist*)
let rec get_exit_room adv exits_li exit_name :room=
  match exits_li with
  |[]-> raise (UnknownExit exit_name)
  |h::t -> if h.name = exit_name then get_room adv.rooms h.where_to 
    else get_exit_room adv t exit_name
(** Exposed Function*)
let next_room adv room ex =
  if not (List.mem room (room_ids adv)) then raise (UnknownRoom room)
  else let actual_room = get_room adv.rooms room in 
    (get_exit_room adv actual_room.exits ex).id


(** [gimme_exit_room_id exit] outputs the room id of an exit *)
let gimme_exit_room_id exit :room_id=
  exit.where_to
let next_rooms adv room =
  let actual_room = get_room adv.rooms room in
  let xt_list = actual_room.exits in
  make_setlike [] (List.map gimme_exit_room_id xt_list)



(*Helpers for State*)

let room_list adv = adv.rooms
let exit_list room = (room.exits:exit list)
let rec aide rooms room_id =
  match rooms with
  |[] -> failwith ("Invalid Room")
  |h::t -> if h.id = room_id then h else aide t room_id
let rec helper exits exit_name =
  match exits with 
  |[] -> "Invalid Exit"
  |h::t -> if h.name = exit_name then h.where_to else helper t exit_name



let items adv =
  adv.items

let item_location item =
  item.location

let rec what_item items item_id =
  match items with
  |[] -> raise(UnknownItem item_id)
  |h::t -> if h.item_id = item_id then h else what_item t item_id


let create_item id location target score = 
  {
    item_id = id;
    location = location;
    score = score;
    target = target;
  }

let rec pick_up list item=
  match list with
  |[] -> print_endline("item doesn't exist"); list
  |h::t -> if h.item_id = item.item_id then 
      (create_item item.item_id "inventory" item.target item.score) :: t
    else h::pick_up t item

let rec drop list item room_id=
  match list with
  |[] -> list
  |h::t -> if h.location = "inventory" && item.item_id = h.item_id then 
      (create_item item.item_id room_id item.target item.score) :: t
    else h :: drop t item room_id