type team = White | Black
type rank = King | Queen | Bishop | Rook | Knight | Pawn
type location = (char*int)
(* Option for location because if a piece is deleted, it shouldn't have a location *)

type piece = {
  team: team;
  rank: rank;
  location: location;
  first_move: bool;
}

(** [get_team] returns the team of [piece]. *)

let get_team piece = 
  piece.team

(** [get_rank] returns the rank of piece [piece] *)
let get_rank piece=
  piece.rank

(** [get_location] returns the position of piece [piece] *)
let get_location piece =
  piece.location

let get_first_move piece = 
  piece.first_move

let equal piece1 piece2 =
  piece1.team = piece2.team &&
  piece1.rank = piece2.rank &&
  piece1.location = piece2.location &&
  piece1.first_move = piece2.first_move

(** [set_piece] returns piece with [location], [rank],[team],
    [firstmove]. *)
let set_piece location rank team firstmove= {
  location = location;
  rank = rank;
  team=team;
  first_move= firstmove;
}

let okay_move (s1, s2) (f1, f2) rank =
  match rank with
  | Pawn -> s1 = f1 && f2 - 1 = s2
  | Rook -> s1 = f1
  | Queen -> s1 = f1 || s2 = f2 || abs (s2 - f2) = abs (Char.code(s1) - Char.code(f1))
  | King -> (s1 = f1 && abs (s2 - f2) = 1) || (s2 = f2 && abs (Char.code(s1) - Char.code(f1)) = 1) || (abs (s2 - f2) = 1 && abs (Char.code(s1) - Char.code(f1)) = 1)
  | Knight -> (abs (Char.code(s1) - Char.code(f1)) = 1 && abs (s2 - f2) = 2) || (abs (Char.code(s1) - Char.code(f1)) = 2 && abs (s2 - f2) = 1)
  | Bishop -> abs (Char.code(s1) - Char.code(f1)) = abs (s2 - f2)

(** [blockable_movement] returns true if [p] is a piece with a blockable
    movement scheme. This means it is a Rook, Queen, Bishop. 
    includes Rook, Bishop, and Queen. Otherwise, it will return false. *)
let blockable_movement p = 
  if (get_rank p = Rook || get_rank p = Queen || get_rank p = Bishop)
  then true else false

let column_from_location (location: location) : char =
  match location with |(x,_) -> x
let row_from_location (location:location) : int = 
  match location with |(_,x) -> x
(** [rank_to_string] returns string version of [rank]. *)
let rank_to_string rank= 
  match rank with
  | Pawn -> "pawn"
  | King -> "king"
  | Queen -> "queen"
  | Knight -> "knight"
  | Rook -> "rook"
  | Bishop -> "bishop"

(** [string_to_rank] returns the rank of [string]. *)
let string_to_rank (string:string) : rank option = 
  match string with
  | "pawn" -> Some Pawn
  | "king" -> Some King
  | "queen" -> Some Queen
  | "knight" -> Some Knight
  | "rook" -> Some Rook
  | "bishop" -> Some Bishop
  | _ -> None


