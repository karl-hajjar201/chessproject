open Piece


type move_set = Piece.location * Piece.location


type command = 
  | Pawn of move_set
  | Knight of move_set
  | Rook of move_set
  | King of move_set
  | Queen of move_set
  | Bishop of move_set
  | Forfeit
  | Taken
  | Score

exception Empty
exception Malformed

(** used for the test.ml file so be able to make a command for test cases.
    make_com takes a [name] of a type of piece and a move_set [ms]
    Requires: name is a string and ms is a move_set*)
let make_com name ms = 
  match name with
  |"pawn" -> Pawn ms
  |"rook" -> Rook ms
  |"bishop" -> Bishop ms
  |"king" -> King ms
  |"queen" -> Queen ms
  |"knight" -> Knight ms
  |_-> failwith("invalid command pairing")


let first_from_moveset (ms: move_set) : Piece.location =
  match ms with |( x,_) -> x

let second_from_moveset (ms: move_set) : Piece.location = 
  match ms with | (_,x) ->x

(** returns true iff p is a valid location between a1 and h8. 
    Requires: p is a Piece.location*)
let is_location p = 
  match p with
  | (l,n) -> Char.code (l) >= 65 && Char.code (l) <= 72 && n >= 1 && n <= 8

(** returns true iff ms is a valid move_set*)
let is_valid_move_set (ms:move_set) =
  match ms with 
  |(l1,l2) -> is_location l1 && is_location l2 


(** Takes a string str and returns a location representation of the string.
    returns a void location if the string is an invalid location
    Requires: str is a string*)
let make_location str =
  let str_list = List.filter (fun x -> x != " ") 
      (String.split_on_char ' ' (String.lowercase_ascii str)) in
  match str_list with
  | x::[] -> (x.[0], int_of_char(x.[1]) - 48)
  | _ -> ('z' ,0) (**set the location to be the designated void location *)

(**returns a move_set representation of the string list sl.
   raises Malformed if the string list is an invalid move_set 
   Requires: sl is a string list*)
let make_move_set (sl:string list)  =
  match sl with
  | [current_l; next_l] -> (make_location current_l, make_location next_l)
  | []-> raise Malformed
  |  _ -> raise Malformed

(** Returns a command representation of string and move_set.
    Raises malformed if the string and or moveset is invalid.
    Requires: string is a string and move_set is a move_set*)
let peice_match string move_set =
  if "forfeit" = string then Forfeit else if "taken" = string 
  then Taken else if "score" =string then Score else
    try let moves = make_move_set move_set in 
      match string with
      | "knight" -> Knight moves
      | "king" -> King moves
      | "pawn" -> Pawn moves
      | "queen" -> Queen moves
      | "bishop" -> Bishop moves
      | "rook" -> Rook moves
      | a -> raise Malformed
    with Malformed -> raise Malformed


let parse str =
  let str_list = List.filter (fun x -> not (x = "")) 
      (String.split_on_char ' ' (String.lowercase_ascii str)) in
  match str_list with
  | [] -> raise Empty
  | h::t -> try peice_match h t with Malformed -> raise Malformed