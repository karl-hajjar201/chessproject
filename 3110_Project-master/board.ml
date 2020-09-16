open Piece
open State

(** [space] is the representation of a tile on the chess board. It has 
    [location], which represents where on the board it lies, and [color], which 
    represents which color the tile will be printed as in the terminal. *)
type space = {
  location : location;
  color : ANSITerminal.color;
}

(** [row] represents each row on the board. It has [number] which represents 
    which number row it is, and also has [tile], which is a 
    [(space * Piece.piece option) list] representing each of the tiles in the 
    row and which piece is located there, if at all. *)
type row = {
  number : int;
  tiles : (space * Piece.piece option) list;
}

type b = {
  current_setup : row list;
}
(** [columns] is a list of charecters representing all the columns in a regular 
    chess board. *)
let columns = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h']

(** [rows] is a list of int representing all the rows in a regular chess board.
*)
let rows = [1; 2; 3; 4; 5; 6; 7; 8]

(** [first tuple] is the first entry in [tuple]. Ex: [first (1, 2)] is 1. 
    Requires: [tuple] has type [a' * a']. *)
let first = function 
  |(x,_) -> x

(** [second tuple] is the first entry in [tuple]. Ex: [first (1, 2)] is 1. 
    Requires: [tuple] has type [a' * a']. *)
let second = function 
  |(_,x) -> x

(** [option_rank option] is the string of the first letter of the rank of the 
    piece in [option] if option is [Some x] and is the string of a space 
    otherwise. Requires: [option] has type [Piece option].  *)
let option_rank = function 
  |Some x -> (match get_rank x with 
      |Pawn -> "p" 
      |Knight -> "k" 
      |Rook -> "R" 
      |King -> "K" 
      |Queen -> "Q" 
      |Bishop -> "B") 
  |None -> " "

(** [space_to_number space] is the number of the column corresponding to 
    [space]. In other words, a space in column a is 1, a space in column b is 
    2, etc. Requires: [space] has type [Piece.space]. *)
let space_to_number space = Char.code (first space.location) - 96

(** [color location] is the ANSITerminal.color of [location] which is a space 
    on the chess board. Each location is translated to either a Blue space or a 
    Red space. Requires: [location] has type [location]. *)
let color (location : location) : ANSITerminal.color =
  match ((second location) + Char.code (first location) - 96) mod 2 with 
  | 0 -> Blue
  | 1 -> Red
  | x -> failwith "Error43"

(** [create_row state number] is the row number [number] in [state]. Requires: 
    [state] has type [State.t] and [number] has type [int] that is greater than
     0 and less than 9. *)
let create_row (state : State.t) number = 
  let map = (fun column list -> ({
      location = (column, number); 
      color = color (column, number)}, 
      State.contents state (column, number)) :: list) in 
  {number = number; tiles = List.fold_right map columns []}

(** [create_board state] is the board of [state]. Requires: [state] has type 
    [State.t]. *)
let create_board state = List.fold_left 
    (fun list number -> create_row state number :: list) [] rows

(** [team_color item] is ANSITerminal.color of *)
let team_color item = 
  match second item with
  | Some x -> begin
      match get_team x with 
      | Black -> ANSITerminal.Black 
      | White -> ANSITerminal.White
    end
  | None -> ANSITerminal.Black

let print_board state = 
  let board = create_board state in
  print_newline ();
  List.iter (fun row -> ANSITerminal.print_string
                [Foreground Yellow] (Int.to_string row.number ^ " "); 
              List.iter (fun item -> ANSITerminal.print_string 
                            [Background (first item).color; 
                             Foreground (team_color item)] 
                            (option_rank (second item) ^ " ")) row.tiles;
              print_newline ()) board;
  ANSITerminal.print_string [Foreground Yellow] "  A B C D E F G H";
  print_newline ()



