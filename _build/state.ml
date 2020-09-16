(* Note: You may introduce new code anywhere in this file. *) 
open Piece
open Command

exception Invalid_Move
(* TODO: replace [unit] with a type of your own design. *)
type t = {
  current_team : Piece.team;
  active_pieces: Piece.piece list; 
  (* ^^ is a list of the active pieces in the game *)
}

let init_pieces: Piece.piece list =
  let piece_match x : Piece.rank = match x with | 'a' -> Rook | 'b' -> Knight | 'c' -> Bishop | 'd' -> Queen | 'e' -> King | 'f' -> Bishop | 'g' -> Knight| 'h' -> Rook | _ -> failwith "peice error" in
  let pawn_match bool column : Piece.rank = match bool with | true -> Pawn | false -> piece_match column in
  let row_match team pawn = match team with 
    | White -> if pawn then 2 else 1
    | Black -> if pawn then 7 else 8 in
  let columns = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'] in
  List.fold_left (fun list bool -> (List.fold_left (fun list team -> (List.fold_left (fun list item -> set_piece (item, row_match team bool) (pawn_match bool item) team bool :: list) [] columns) @ list) [] [Black; White]) @ list) [] [true; false]

let init_state ={
  current_team = White;
  active_pieces = init_pieces;
}

<<<<<<< HEAD
let win_condition_black st = 
  failwith("unimplimented")

let win_condition_white st = 
  failwith("unimplimented")
=======

let win_condition_black st = 
  let x = List.filter (fun r -> Piece.get_rank r = King) st.active_pieces in
  match List.length x with
  |1 -> let y = List.hd x in 
    if Piece.get_team y = Black then true else false
  |_ -> false


let win_condition_white st = 
  let x = List.filter (fun r -> Piece.get_rank r = King) st.active_pieces in
  match List.length x with
  |1 -> let y = List.hd x in 
    if Piece.get_team y = White then true else false
  |_ -> false
>>>>>>> 8009092c1941a8292f7a39e52c59de33b9fb3c53

let move com st = 
  failwith("unimplimented")

<<<<<<< HEAD
let win_condition st adv = 
  failwith("unimplimented")
=======
let win_condition st : bool = 
  let b = win_condition_black st in 
  let w = win_condition_white st in 
  if b || w then true else false
>>>>>>> 8009092c1941a8292f7a39e52c59de33b9fb3c53

let contents t loc : Piece.piece option = 
  let x = List.filter (fun r -> Piece.get_location r = loc) t.active_pieces in
  match x with
  | h::[] -> Some h
  | [] -> None
  | h::t -> failwith "Error"

let construct_path ((s1, s2), (f1, f2)) =
  let rec acc = fun (a1, a2) list hor vert -> match (a1, a2) = (f1, f2) with 
    | true -> list
    | false -> acc (Char.chr(Char.code a1 + hor), a2 + vert) ((a1, a2) :: list) hor vert in
  match Int.abs (Char.compare s1 f1) > 0 with
  | false -> begin 
      match s2 = f2 with 
      | true -> []
      | false -> if s2 > f2 then acc (s1, s2 - 1) [] 0 (-1) else acc (s1, s2 + 1) [] 0 1
    end
  | true -> begin
      match Char.compare s1 f1 > 0 with 
      | true -> begin
          match s2 = f2 with 
          | true -> acc (Char.chr(Char.code s1 - 1), s2) [] (-1) 0
          | false -> if s2 > f2 then acc (Char.chr(Char.code s1 - 1), s2 - 1) [] (-1) (-1) else acc (Char.chr(Char.code s1 - 1), s2 + 1) [] (-1) 1
        end
      | false -> begin
          match s2 = f2 with 
          | true -> acc (Char.chr(Char.code s1 + 1), s2) [] 1 0
          | false -> if s2 > f2 then acc (Char.chr(Char.code s1 + 1), s2 - 1) [] 1 (-1) else acc (Char.chr(Char.code s1 + 1), s2 + 1) [] 1 1
        end
    end

let clear_path t path : bool = 
  List.length( List.filter (fun a -> contents t a = None) (construct_path path)) = List.length (construct_path path) 

let get_current_team t = 
  match t.current_team with 
  | White -> "White team, "
  | Black -> "Black team, "

let rec next_turn old_peice new_peice piece_list = 
  match piece_list with 
  | h :: t -> if Piece.equal old_peice h then new_peice :: t else h :: next_turn old_peice new_peice t
  | [] -> failwith "Didn't work"

let move t rank start finish =
  match contents t start with 
  | Some x -> begin 
      match (get_team x = t.current_team) && (get_rank x = rank) && (if rank = Knight then true else clear_path t (start, finish)) with
      | true -> next_turn x (set_piece finish rank t.current_team false) t.active_pieces
      | false -> raise Invalid_Move
    end
  | None -> raise Invalid_Move

let go command t : t =
  let new_team = match t.current_team with 
    | White -> Black
    | Black -> White in
  let new_piece_list = match command with
    | Pawn (s, f) -> assert (okay_move s f Pawn); move t Pawn s f
    | Knight (s, f) -> assert (okay_move s f Knight); move t Knight s f
    | Rook (s, f) -> assert (okay_move s f Rook); move t Rook s f
    | King (s, f) ->  assert (okay_move s f King); move t King s f
    | Queen (s, f) -> assert (okay_move s f Queen); move t Queen s f
    | Bishop (s, f) -> assert (okay_move s f Bishop); move t Bishop s f
    | Forfit -> [] in
  {current_team = new_team; active_pieces = new_piece_list}

(** Take in a command, Rank and (location, location)
    check if there is actually the right piece at the first move set
    check that the piece type can move to the second move set*)

(* if it's good, change the piece's location, give back a new state with the changes reflected in the piece list *)