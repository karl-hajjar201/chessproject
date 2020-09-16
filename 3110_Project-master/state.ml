(* Note: You may introduce new code anywhere in this file. *) 
open Piece
open Command

exception Invalid_Move
exception Checkmate

(* TODO: replace [unit] with a type of your own design. *)
type t = {
  current_team : Piece.team;
  active_pieces: Piece.piece list; 
  check_white : bool;
  check_black : bool;
}

let init_pieces: Piece.piece list = 
  let piece_match x : Piece.rank = 
    match x with | 'a' -> Rook | 'b' -> Knight | 'c' -> Bishop | 'd' -> Queen 
                 | 'e' -> King | 'f' -> Bishop | 'g' -> Knight | 'h' -> Rook 
                 | _ -> failwith "peice error" in
  let pawn_match bool column : Piece.rank = 
    match bool with 
    | true -> Pawn | false -> piece_match column in
  let row_match team pawn = match team with 
    | White -> if pawn then 2 else 1 | Black -> if pawn then 7 else 8 in
  let columns = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'] in
  List.fold_left (fun list bool -> 
      (List.fold_left 
         (fun list team -> 
            (List.fold_left 
               (fun list item -> 
                  set_piece 
                    (item, row_match team bool) (pawn_match bool item) team bool 
                  :: list) [] columns) @ list) [] [Black; White]) @ list) 
    [] [true; false]


let init_state ={
  current_team = White;
  active_pieces = init_pieces;
  check_white = false;
  check_black = false;
}

let make_state t p cw cb = {
  current_team= t;
  active_pieces= p;
  check_black = cb;
  check_white = cw;
}

let active_pieces t =
  t.active_pieces

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

(** [win_condition st] is true if white or black has won the game, and is false
    otherwise. Requires [st] is of type [State.t]. *)
let win_condition st : bool = 
  let b = win_condition_black st in 
  let w = win_condition_white st in 
  if b || w then true else false

let contents t loc : Piece.piece option = 
  let x = List.filter (fun r -> Piece.get_location r = loc) t.active_pieces in
  match x with
  | h::[] -> Some h
  | [] -> None
  | h::t -> failwith "Error14"

(** [acc location finish list hor vert] is a helper function for 
    [construct_path start finish]. It adds the current location to list if 
    location != finish, and then calls the function again with location 
    incremented by hor and vert. Requires: [location] and [finish] is of type
    [location], [list] has type [location list], [hor] and [vert] have type 
    [int]. *)
let rec acc = fun (a1, a2) (f1, f2) list hor vert -> match (a1, a2) = (f1, f2) 
  with 
  | true -> list
  | false -> 
    acc (Char.chr(Char.code a1 + hor), a2 + vert) (f1, f2) ((a1, a2) :: list) 
      hor vert 

(** [construct_path_helper_true (start, finish)] is a helper function for 
    [construct_path start finish] which constructs [location list] which is the
    path from [start] to [finish] if [Char.compare s1 f1 > 0] (not including 
    start and finish). Requires: [start] and [finish] have type [location].*)
let construct_path_helper_true ((s1, s2), (f1, f2)) = 
  match s2 = f2 with 
  | true -> acc (Char.chr(Char.code s1 - 1), s2) (f1, f2) [] (-1) 0
  | false -> begin  match s2 > f2 with
      | true -> acc (Char.chr(Char.code s1 - 1), s2 - 1) (f1, f2) [] (-1) (-1) 
      | false -> acc (Char.chr(Char.code s1 - 1), s2 + 1) (f1, f2) [] (-1) 1
    end

(** [construct_path_helper_false (start, finish)] is a helper function for 
    [construct_path start finish] which constructs [location list] which is the
    path from [start] to [finish] if [Char.compare s1 f1 < 0] (not including 
    start and finish). Requires: [start] and [finish] have type [location].  *)
let construct_path_helper_false ((s1, s2), (f1, f2)) =
  match s2 = f2 with 
  | true -> acc (Char.chr(Char.code s1 + 1), s2) (f1, f2) [] 1 0
  | false -> begin match s2 > f2 with 
      | true -> acc (Char.chr(Char.code s1 + 1), s2 - 1) (f1, f2) [] 1 (-1) 
      | false -> acc (Char.chr(Char.code s1 + 1), s2 + 1) (f1, f2) [] 1 1
    end

(** [construct_path (start, finish)] is a list of locations in between [start]
    and [finish]. Requires [start] and [finish] have type [location]. *)
let construct_path ((s1, s2), (f1, f2)) = 
  match Int.abs (Char.compare s1 f1) > 0 with
  | false -> begin 
      match s2 = f2 with 
      | true -> []
      | false -> if s2 > f2 then acc (s1, s2 - 1) (f1, f2) [] 0 (-1) 
        else acc (s1, s2 + 1) (f1, f2) [] 0 1
    end
  | true -> match (Char.compare s1 f1) > 0 with 
    | true -> construct_path_helper_true ((s1, s2), (f1, f2))
    | false -> construct_path_helper_false ((s1, s2), (f1, f2))

(** [clear_path t path] is true if there are not any pieces in every space in
    [path] in state [t] and is false otherwise. [path] is a start and a finish, 
    so [clear_path t path] checks all the spaces in between, but not including 
    start and finish. Requires: [t] has type [State.t] and [path] has type
     [location * location]. *)
let clear_path t path : bool = 
  List.length (List.filter 
                 (fun item -> contents t item = None) 
                 (construct_path path)) = List.length (construct_path path) 

let get_current_team t = 
  match t.current_team with 
  | White -> "White team, "
  | Black -> "Black team, "

(** [piece_replace old_piece new_piece piece_list] is the [piece_list], but 
    removes [old_piece] and inserts [new_piece] in [piece_list]. Raises: Failure
    if [old_piece] is not in [piece_list].Requires: [old_piece] and [new_piece]
     have type [Piece.piece] and [piece_list] has type [Piece.piece list]. *)
let rec piece_replace old_peice new_peice piece_list = 
  match piece_list with 
  | h :: t -> if Piece.equal old_peice h then new_peice :: t 
    else h :: piece_replace old_peice new_peice t
  | [] -> failwith "Didn't work"

(** [filter_check piece t location x] is a helper function for 
    [check_detection t] which is true if moving [piece] from [location] to [x] 
    is allowed under the traditional rules of chess, and is false otherwise. 
    Requires: [piece] has type [Piece.piece], [t] has type [State.t], 
    [location] and [x] have type [location]. *)
let filter_check = (fun piece t location x -> 
    (if (Piece.get_rank piece != Knight) then clear_path t (location, x) 
     else true) && begin 
      match contents t x with 
      | Some y -> if Piece.get_team y != Piece.get_team piece 
        then Piece.okay_move location x piece true else false
      | None -> Piece.okay_move location x piece false
    end)

let check_detection t = 
  let acc = fun list piece -> 
    let possible_moves = Piece.get_possible_space piece in
    let location = Piece.get_location piece in 
    list @ (List.filter (filter_check piece t location) possible_moves) 
  in
  let opposite_team = 
    List.filter (fun z -> Piece.get_team z != t.current_team) t.active_pieces in
  let all_moves_1 = List.fold_left acc [] opposite_team in
  let all_moves = List.filter (fun n -> contents t n != None) all_moves_1 in
  List.length (List.filter (fun m -> begin 
        match (contents t m) with 
        | Some w -> get_rank w = King && get_team w = t.current_team
        | None -> failwith "error654"
      end) all_moves) != 0

(** [move t rank start finish] is the updated piece list when the piece at 
    [start] with [rank] is moved to [finish] in state [t]. Raises: 
    [Invalid_move] if the piece is not there or does not have rank [rank] and 
    if there is not a clear path in between [start] and [finish]. Requires:
    [t] has type [State.t], [rank] has type [Piece.rank], [start] and [finish] 
    has type [location]. *)
let move t rank start finish =
  try begin 
    match contents t start with 
    | Some x -> begin 
        match (get_team x = t.current_team) && (get_rank x = rank) && 
              (if rank = Knight then true else clear_path t (start, finish)) 
        with
        | true -> piece_replace x 
                    (set_piece finish rank t.current_team false) t.active_pieces
        | false -> raise Invalid_Move
      end
    | None -> raise Invalid_Move end
  with Failure ex -> raise Invalid_Move

(** [move_calling t rank start finish] is the updated piece list of [t] when 
    the piece at [start] is moved to [finish] in state [t]. [move_calling] acts
    similarly to [move] but instead, takes into account whether or not there
    is a piece present at [finish]. Raises: [Invalid_move] if the piece in 
    [finish] is the same team, if the piece is not at [start] or if the piece
    at [start] does not have rank [rank]. *)
let move_calling t rank start finish = 
  match contents t finish with 
  | Some x -> if Piece.get_team x != t.current_team 
    then move 
        {current_team = t.current_team; active_pieces = List.filter 
                                            (fun p -> not(equal p x)) 
                                            t.active_pieces; 
         check_white = false; check_black = false} rank start finish
    else raise Invalid_Move
  | None -> move t rank start finish

(** [get_piece t location] is the piece at [location] in state [t]. Raises: 
    [Failure] if there is no piece present or if there is more than one piece
    at [location]. Requires: [t] has type [State.t] and [location] has type
    [location]. *)
let get_piece t location = 
  match List.filter (fun x -> get_location x = location) t.active_pieces with 
  | h :: [] -> h
  | _ -> failwith "error8540"

(** [assert_good_move t rank s f taking] is true if the piece at [s] has rank 
    [rank], is the same team as the current team of [t], and if the move is 
    valid under traditional chess rules from [s] to [f]. Raises: [Invalid_move]
    if the piece at [s] does not have rank [rank] or if it is not apart of the 
    same team as the current team of t, and if the move from [s] to [f] is okay
    in state [t] under traditional chess rules. Also, if there is not piece at 
    [s], then [Invalid_move] is raised. *)
let assert_good_move t rank s f taking = 
  try let piece = get_piece t s in 
    match (get_rank piece = rank && get_team piece = t.current_team && 
           okay_move s f piece taking) with 
    | true -> ()
    | false -> raise Invalid_Move
  with Failure _ -> raise Invalid_Move

(** [new_team t] is the team that is not the current team in [t]. Requires:
    [t] has type [State.t]. *)
let new_team t = match t.current_team with 
  | White -> Black
  | Black -> White 

(** [th rank team r] is a helper function for [black_taken], [white_taken], 
    [black_score], and [white_score]. It is true if the rank of [r] is [rank] 
    and if the team of [r] is team. Otherwise, it is false. 
    Requires: [rank] has type [Piece.rank], [team] has type [Piece.team], and
    [r] has type [Piece.piece]. *)
let th (rank : Piece.rank) team r = Piece.get_rank r = rank && Piece.get_team r
                                                               = team

let black_taken act_list = 
  let num_pawns = 8 - List.length (List.filter (th Pawn Black) act_list) in
  let num_bishops = 2 - List.length(List.filter (th Bishop Black) act_list) in
  let num_rooks = 2 - List.length(List.filter (th Rook Black) act_list) in
  let num_knights = 2 - List.length(List.filter (th Knight Black) act_list) in
  let num_queens = 1 - List.length(List.filter (th Queen Black) act_list) in
  let taken_assoc = [("pawns",num_pawns);("bishops",num_bishops);
                     ("rooks",num_rooks);("knights",num_knights);
                     ("queens",num_queens)]
  in taken_assoc

let white_taken act_list =
  let num_pawns = 8 - List.length (List.filter (th Pawn White) act_list) in
  let num_bishops = 2 - List.length(List.filter (th Bishop White) act_list) in
  let num_rooks = 2 - List.length(List.filter (th Rook White) act_list) in
  let num_knights = 2 - List.length(List.filter (th Knight White) act_list) in
  let num_queens = 1 - List.length(List.filter (th Queen White) act_list) in
  let taken_assoc = [("pawns",num_pawns);("bishops",num_bishops);
                     ("rooks",num_rooks);("knights",num_knights);
                     ("queens",num_queens)]
  in taken_assoc



let black_score act_list =
  let score_pawns = 8 - List.length (List.filter (th Pawn White) act_list) in
  let score_bishops =3*( 2 - List.length(List.filter (th Bishop White) act_list
                                        )) in
  let score_rooks = 5 *(2 - List.length(List.filter (th Rook White) act_list)) 
  in
  let score_knights =3*( 2 - List.length(List.filter (th Knight White) act_list
                                        )) in
  let score_queens =9*( 1 - List.length(List.filter (th Queen White) act_list)) 
  in
  let black_score = score_pawns +score_bishops + score_rooks + score_knights 
                    +score_queens in
  black_score

let white_score act_list =
  let score_pawns = 8 - List.length (List.filter (th Pawn Black) act_list) in
  let score_bishops =3*( 2 - List.length(List.filter (th Bishop Black) act_list
                                        )) in
  let score_rooks = 5 *(2 - List.length(List.filter (th Rook Black) act_list)) 
  in
  let score_knights =3*( 2 - List.length(List.filter (th Knight Black) act_list
                                        )) in
  let score_queens =9*( 1 - List.length(List.filter (th Queen Black) act_list)) 
  in
  let white_score = score_pawns +score_bishops + score_rooks + score_knights 
                    +score_queens in
  white_score

(** [go_helper t s f rank bool] is the new piece list of all pieces in [t] for 
    the move from [s] to [f]. Raises: [Invalid_move] if rank != the rank of the
    piece at [s] and if the piece at [s] cannot move from [f]. Also raises 
    [Invalid_move] if there is not a piece at [s]. *)
let go_helper t s f rank bool = 
  assert_good_move t rank s f bool; move_calling t rank s f

(** [go command t] is the state that results from [command] being executed in
    state [t]. Raises: [Invalid_move] if [command] is not a valid move in state
    [t] in the classic rules of chess. *)
let go command (t : t) : t =
  let new_piece_list = match command with
    | Pawn (s, f) -> begin 
        match contents t f with 
        | Some x -> assert_good_move t Pawn s f true 
        | None -> assert_good_move t Pawn s f false 
      end; move_calling t Pawn s f
    | Knight (s, f) -> go_helper t s f Knight false
    | Rook (s, f) -> go_helper t s f Rook false
    | King (s, f) ->  go_helper t s f King false
    | Queen (s, f) -> go_helper t s f Queen false
    | Bishop (s, f) -> go_helper t s f Bishop false
    | Taken -> t.active_pieces
    | Score -> t.active_pieces 
    | Forfeit -> [] in
  {current_team = (new_team t); active_pieces = new_piece_list; 
   check_white = false; check_black = false}

(** [go command t] is the state that results from [command] being executed in
    state [t], except that the team that made the move is still the current team 
    in the new state. Raises: [Invalid_move] if [command] is not a valid move in 
    state [t] in the classic rules of chess. *)
let go_same_team command (t : t) : t = 
  let new_piece_list = match command with
    | Pawn (s, f) -> begin 
        match contents t f with 
        | Some x -> assert_good_move t Pawn s f true 
        | None -> assert_good_move t Pawn s f false 
      end; move_calling t Pawn s f
    | Knight (s, f) -> go_helper t s f Knight false
    | Rook (s, f) -> go_helper t s f Rook false
    | King (s, f) ->  go_helper t s f King false
    | Queen (s, f) -> go_helper t s f Queen false
    | Bishop (s, f) -> go_helper t s f Bishop false
    | Taken -> t.active_pieces 
    | Score -> t.active_pieces
    | Forfeit -> [] in
  {current_team = t.current_team; active_pieces = new_piece_list; 
   check_white = false; check_black = false}

(** [filter_checkmate piece location t possible_moves] is a tuple containing 
    [piece] in the first entry, and a list of locations that the [piece] can
    move to in state [t] if the [piece] is at [location] and has possible moves
    [possible_moves]. *)
let filter_checkmate piece location t possible_moves = 
  (piece, List.filter (fun x -> 
       (if (Piece.get_rank piece != Knight) 
        then clear_path t (location, x) 
        else true) && begin 
         match contents t x with 
         | Some y -> if Piece.get_team y != Piece.get_team piece 
           then Piece.okay_move location x piece true 
           else false
         | None -> Piece.okay_move location x piece false
       end) possible_moves)

let fold_p = fun ploc t bool xloc -> 
  not(check_detection 
        (go_same_team (Command.Pawn (ploc, xloc)) t)) || bool

let fold_r = fun ploc t bool xloc -> 
  not(check_detection 
        (go_same_team (Command.Rook (ploc, xloc)) t)) || bool

let fold_k = fun ploc t bool xloc -> 
  not(check_detection 
        (go_same_team (Command.Knight (ploc, xloc)) t)) || bool

let fold_b = fun ploc t bool xloc -> 
  not(check_detection 
        (go_same_team (Command.Bishop (ploc, xloc)) t)) || bool

let fold_q = fun ploc t bool xloc -> 
  not(check_detection 
        (go_same_team (Command.Queen (ploc, xloc)) t)) || bool

let fold_king = fun ploc t bool xloc -> 
  not(check_detection 
        (go_same_team (Command.King (ploc, xloc)) t)) || bool

let checkmate_detection t = 
  let acc = fun list piece -> 
    let possible_moves = Piece.get_possible_space piece in
    let location = Piece.get_location piece in 
    (filter_checkmate piece location t possible_moves) :: list in
  let this_team = List.filter 
      (fun z -> Piece.get_team z = t.current_team) t.active_pieces in
  let all_moves_1 = List.fold_left acc [] this_team in
  let not_check_moves = List.filter (fun (p, list) ->
      let ploc = Piece.get_location p in 
      match Piece.get_rank p with 
      | Pawn -> List.fold_left (fold_p ploc t) false list
      | Rook -> List.fold_left (fold_r ploc t) false list
      | Knight -> List.fold_left (fold_k ploc t) false list
      | Bishop -> List.fold_left (fold_b ploc t) false list
      | Queen -> List.fold_left (fold_q ploc t) false list
      | King -> List.fold_left (fold_king ploc t) false list
    ) all_moves_1 in 
  List.length not_check_moves = 0


