(** 
   Representation of static adventure data.

   This module represents the data stored in adventure files, including
   the rooms and exits.  It handles loading of that data from JSON as well
   as querying the data.
*)

(* You are free to add more code here. *)

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The abstract type of values representing adventures. *)
type t

(** The type of room identifiers. *)
type room_id = string
type item_id = string
type item

(** The type of exit names. *)
type exit_name = string

(** Raised when an unknown room is encountered. *)
exception UnknownRoom of room_id

(** Raised when an unknown exit is encountered. *)
exception UnknownExit of exit_name

exception UnknownItem of item_id
(** [from_json j] is the adventure that [j] represents.
    Requires: [j] is a valid JSON adventure representation. *)
val from_json : Yojson.Basic.t -> t

(** [start_room a] is the identifier of the starting room in adventure 
    [a]. *)
val start_room : t -> room_id

val target_room : t -> room_id


(** [room_ids a] is a set-like list of all of the room identifiers in 
    adventure [a]. *)
val room_ids : t -> room_id list

(** [description a r] is the description of room [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val description : t -> room_id -> string

(** [exits a r] is a set-like list of all exit names from room [r] in 
    adventure [a].
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val exits : t -> room_id -> exit_name list

(** [next_room a r e] is the room to which [e] exits from room [r] in
    adventure [a].  
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].
    Raises [UnknownExit e] if [e] is not an exit from room [r] in [a]. *)
val next_room : t -> room_id -> exit_name -> room_id

val in_inventory : item -> bool
(** [next_rooms a r] is a set-like list of all rooms to which there is an exit 
    from [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].*)
val next_rooms : t -> room_id -> room_id list


(* END DO NOT CHANGE
 **********************************************************************)
(* Defining abstrack type exit*)
type exit
(* Defining abstrack type room*)
type room
(** [room_list adv] gives the room list of the adv [adv]*)
val room_list : t -> room list
(** [exit_list room] gives the exits_name list of the room [room]*)
val exit_list : room -> exit list
(** [aide rooms room_id] gives the room with given room_id [room_id]*)
val aide : room list -> room_id -> room
(** [helper exits exit_name] gives the room_id that an exit of name 
    [exit_name] leads to *)
val helper : exit list -> exit_name -> room_id

(** *)
val get_score : t -> room_id -> int

val  item_id_ls : item list -> item_id list

val items : t -> item list

val item_location : item -> room_id

val in_room: room_id -> item -> bool

val pick_up : item list -> item -> item list

val drop : item list -> item -> room_id -> item list

val what_item : item list -> item_id -> item

val item_id: item -> item_id
(* You are free to add more code here. *)
