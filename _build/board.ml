open Piece
open State

type space = {
  location : location;
  color : ANSITerminal.color;
}

type row = {
  number : int;
  tiles : (space * Piece.piece option) list;
}

type b = {
  current_setup : row list;
}

type empty = None

let columns = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h']
let rows = [1; 2; 3; 4; 5; 6; 7; 8]

let first = function 
  |(x,_) -> x
let second = function 
  |(_,x) -> x
let option_rank = function 
  |Some x -> (match get_rank x with 
      |Pawn -> "p" 
      |Knight -> "k" 
      |Rook -> "R" 
      |King -> "K" 
      |Queen -> "Q" 
      |Bishop -> "B") 
  |None -> " "

let space_to_number space = Char.code (first space.location) - 96

let color (location : location) : ANSITerminal.color =
  match ((second location) + Char.code (first location) - 96) mod 2 with 
  | 0 -> Blue
  | 1 -> Red
  | x -> failwith "Error"

let create_row (state : State.t) number = 
  let map = (fun column list -> ({
      location = (column, number); color = color (column, number)}, State.contents state (column, number)) :: list) in 
  {number = number; tiles = List.fold_right map columns []}

let create_board state = List.fold_left (fun list number -> create_row state number :: list) [] rows

let team_color item = 
  match second item with
  | Some x -> begin
      match get_team x with 
      | Black -> ANSITerminal.Black 
      | White -> ANSITerminal.White
    end
  | None -> ANSITerminal.Black

(* [({location = ('a', number); color = color location}, None); ({location = ('a',2); color = color location}, None); ({location = ('a',3); color = color location}, None); ({location = ('a',4); color = color location}, None); ({location = ('a',5); color = color location}, None); ({location = ('a',6); color = color location}, None); ({location = ('a',7); color = color location}, None); ({location = ('a',8); color = color location}, None);]}
   } *)
(* move_set
   List.fold_left (fun board row -> {number = row.number; tiles = List.fold_left (fun tiles element -> (first element, List.nth pieces (row.number + space_to_number element))::spaces) [] row.tiles)}::board) [] b.current_setup *)

let print_board state = 
  let board = create_board state in
  print_newline ();
  List.iter (fun row -> ANSITerminal.print_string [Foreground Yellow] (Int.to_string row.number ^ " "); 
              List.iter (fun item -> ANSITerminal.print_string [Background (first item).color; Foreground (team_color item)] (option_rank (second item) ^ " ")) row.tiles; print_newline ()) board;
  ANSITerminal.print_string [Foreground Yellow] "  A B C D E F G H";
  print_newline ()



