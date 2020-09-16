open Piece

type move_set = Piece.location * Piece.location


type command = 
  | Pawn of move_set
  | Knight of move_set
  | Rook of move_set
  | King of move_set
  | Queen of move_set
  | Bishop of move_set
  | Forfit

exception Empty
exception Malformed

let first_from_moveset (ms: move_set) : Piece.location =
  match ms with |( x,_) -> x

let second_from_moveset (ms: move_set) : Piece.location = 
  match ms with | (_,x) ->x

(** this needs to take in a location option but right now is taking in a char * int *)
let is_location p = 
  match p with
  | (l,n) -> Char.code (l) >= 65 && Char.code (l) <= 72 && n >= 1 && n <= 8

let is_valid_move_set (ms:move_set) =
  match ms with 
  |(l1,l2) -> is_location l1 && is_location l2 

let make_location str =
  let str_list = List.filter (fun x -> x != " ") (String.split_on_char ' ' (String.lowercase_ascii str)) in
  match str_list with
  | x::[] -> (x.[0], int_of_char(x.[1]) - 48)
  | _ -> ('z' ,0) (**set the location to be the designated void location *)

(**write this to take in a string list and output a move set. then is check i
   f this is valid and throw a malformed error if it is not a valid move set. then in the parse function we will use a try block to catch this error *)
let make_move_set (sl:string list)  =
  match sl with
  | [current_l; next_l] -> (make_location current_l, make_location next_l)
  | []-> raise Malformed
  |  _ -> raise Malformed

let peice_match string move_set =
  if "forfit" = string then Forfit else
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

(** want to check validity of move set but also realize that Knight t and ect wants a move set but rn is getting a string list. need to make a function that takes a string list and gives a move_set*)
let parse str =
  let str_list = List.filter (fun x -> not (x = "")) (String.split_on_char ' ' (String.lowercase_ascii str)) in
  match str_list with
  | [] -> raise Empty
  | h::t -> try peice_match h t with Malformed -> raise Malformed