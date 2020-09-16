type team = White | Black
type rank = King | Queen | Bishop | Rook | Knight | Pawn
type location = (char*int)

type piece = {
  team: team;
  rank: rank;
  location: location;
  first_move: bool;
  possible_spaces: location list;
}

(** [board] is a list of all locations in a regular chess game board. *)
let board = List.fold_left (fun list x -> 
    list @ (List.fold_left (fun list element -> (x, element) :: list)
              [] [1;2;3;4;5;6;7;8])) [] ['a';'b';'c';'d';'e';'f';'g';'h'] 

let get_team piece = 
  piece.team

let get_rank piece=
  piece.rank

let get_location piece =
  piece.location

let get_first_move piece = 
  piece.first_move

let get_possible_space piece =
  piece.possible_spaces

let equal piece1 piece2 =
  piece1.team = piece2.team &&
  piece1.rank = piece2.rank &&
  piece1.location = piece2.location &&
  piece1.first_move = piece2.first_move

(** [space_maker_helper start finish rank team firstmove] is true
    if a piece at location [start], with rank [rank], team [team] and
    firstmove [firstmove] can move to location [finish], and is false
    otherwise. Requires: [start] and [finish] have type [Piece.location],
    [rank] has type [Piece.rank], [team] has type [Piece.team] and
     [firstmove] has type [bool]. *)
let space_maker_helper (s1, s2) (f1, f2) rank team firstmove =
  match rank with
  | Pawn -> begin 
      match team with
      | White -> (f2 - 1 = s2 && (Char.code s1 - 1 = Char.code f1 ||
                                  Char.code s1 + 1 = Char.code f1)) ||
                 (f1 = s1 && (if firstmove then
                                (f2 - 1 = s2 || f2 - 2 = s2) else f2 - 1 = s2))
      | Black -> (f2 + 1 = s2 &&
                  (Char.code s1 - 1 = Char.code f1 ||
                   Char.code s1 + 1 = Char.code f1)) || 
                 (f1 = s1 && (if firstmove then (f2 + 1 = s2 || f2 + 2 = s2)
                              else f2 + 1 = s2))
    end
  | Rook -> (s1 = f1 && s2 != f2) || (s2 = f2 && f1 != s1)
  | Queen -> s1 = f1 || s2 = f2 || abs (s2 - f2) =
                                   abs (Char.code(s1) - Char.code(f1))
  | King -> (s1 = f1 && abs (s2 - f2) = 1) || (s2 = f2 &&
                                               abs (Char.code(s1) - 
                                                    Char.code(f1)) = 1) ||
            (abs (s2 - f2) = 1 &&
             abs (Char.code(s1) - Char.code(f1)) = 1)
  | Knight -> (abs (Char.code(s1) - Char.code(f1)) = 1 && abs (s2 - f2) = 2)
              || (abs (Char.code(s1) - Char.code(f1)) = 2 && abs (s2 - f2) = 1)
  | Bishop -> abs (Char.code(s1) - Char.code(f1)) = abs (s2 - f2)

let set_piece location rank team firstmove= {
  location = location;
  rank = rank;
  team=team;
  first_move= firstmove;
  possible_spaces= List.filter (fun (x, y) -> 
      Char.code (Char.lowercase_ascii x) > 96
      && Char.code(Char.lowercase_ascii x) < 105 && y < 9 && y > 0)
      (List.filter (fun x -> space_maker_helper location x rank team firstmove)
         board);
}

let string_of_piece p =
  let team = 
    (match (get_team p) with
     |Black -> "Black"
     |White -> "White")
  in
  let rank = 
    (match get_rank p with
     |Pawn -> "pawn"
     |Rook -> "rook"
     |Knight -> "knight"
     |Bishop -> "bishop"
     |Queen -> "queen"
     |King -> "king")
  in
  let loc = 
    (match get_location p with
     |(c,i) -> (Char.escaped c) ^ (string_of_int i)) 
  in
  let first = 
    string_of_bool (get_first_move p)
  in
  team ^ " " ^ rank ^ " at " ^ loc ^ ", " ^ first

let okay_move (s1, s2) (f1, f2) t taking =
  match t.rank with
  | Pawn -> begin 
      match t.team with
      | White -> begin 
          match taking with 
          | true -> f2 - 1 = s2 && Char.code s1 - 1 = Char.code f1 
                    || Char.code s1 + 1 = Char.code f1
          | false -> if t.first_move then s1 = f1 && 
                                          (f2 - 2 = s2 || f2 - 1 = s2) else
              s1 = f1 && f2 - 1 = s2 end
      | Black -> begin 
          match taking with 
          | true -> f2 + 1 = s2 &&
                    Char.code s1 - 1 = Char.code f1 ||
                    Char.code s1 + 1 = Char.code f1
          | false -> if t.first_move then s1 = f1
                                          && (f2 + 2 = s2 || f2 + 1 = s2)
            else s1 = f1 && f2 + 1 = s2 end
    end
  | Rook -> (s1 = f1 && s2 != f2) || (s2 = f2 && f1 != s1)
  | Queen -> s1 = f1 || s2 = f2 || abs (s2 - f2) =
                                   abs (Char.code(s1) - Char.code(f1))
  | King -> (s1 = f1 && abs (s2 - f2) = 1) ||
            (s2 = f2 && abs (Char.code(s1) - Char.code(f1)) = 1) ||
            (abs (s2 - f2) = 1 && abs (Char.code(s1) - Char.code(f1)) = 1)
  | Knight -> (abs (Char.code(s1) - Char.code(f1)) = 1 && abs (s2 - f2) = 2) ||
              (abs (Char.code(s1) - Char.code(f1)) = 2 && abs (s2 - f2) = 1)
  | Bishop -> abs (Char.code(s1) - Char.code(f1)) = abs (s2 - f2)

let blockable_movement p = 
  if (get_rank p = Rook || get_rank p = Queen || get_rank p = Bishop)
  then true else false

let column_from_location (location: location) : char =
  match location with |(x,_) -> x
let row_from_location (location:location) : int = 
  match location with |(_,x) -> x

let rank_to_string rank= 
  match rank with
  | Pawn -> "pawn"
  | King -> "king"
  | Queen -> "queen"
  | Knight -> "knight"
  | Rook -> "rook"
  | Bishop -> "bishop"

let string_to_rank (string:string) : rank option = 
  match string with
  | "pawn" -> Some Pawn
  | "king" -> Some King
  | "queen" -> Some Queen
  | "knight" -> Some Knight
  | "rook" -> Some Rook
  | "bishop" -> Some Bishop
  | _ -> None


