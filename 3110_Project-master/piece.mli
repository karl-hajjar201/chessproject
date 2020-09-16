(** 
   this module is a representation of each individual piece on the chess board.

   Each piece has a rank, team, and location which is used and manipulated to 
   represent a live chess game. 
*)

(** The type of chess piece ranks. *)
type rank = King | Queen | Bishop | Rook | Knight | Pawn

(** The type of the teams. *)
type team = White | Black

(** The type of locations of pieces *)
type location = (char * int)

(** The type of a piece*)
type piece

(** [string_of_piece piece] is the string of [piece]. Ex: If piece is a white 
    pawn at (a, 1) with firstmove as true, then [string_of_piece piece] is 
    ["White pawn at a1, true"]. Requires: [piece] has type [Piece.team]. *)
val string_of_piece : piece -> string

(** [get_team piece] is the team of [piece]. Requires: [piece] has type 
    [Piece.piece]. *)
val get_team: piece -> team


(** [get_location piece] is the location of piece [piece]. Requires: [piece]
    has type [Piece.piece].  *)
val get_location : piece -> location

(** [get_possible_space piece] is all the possible spaces that [piece] can 
    move to (note: this does not take into account an other pieces that may be 
    in the chess game. Requires: [piece] has type [Piece.piece]. *)
val get_possible_space : piece -> location list

(** [get_rank piece] is the rank of piece [piece]. Requires: [piece] has type
    [Piece.piece].  *)
val get_rank : piece -> rank

(** [set_piece] is piece with located at [location], with rank [rank], team
    [team], and firstmove [firstmove]. Requires: [location] has type [location]
    , [rank] has type [Piece.rank], [team] has type [Piece.team], and 
    [firstmove] has type [bool]. *)
val set_piece: location -> rank -> team -> bool -> piece

(** [blockable_movement p] is true if [p] is a piece with a blockable
    movement scheme. This means it is a Rook, Queen, Bishop. 
    includes Rook, Bishop, and Queen. Otherwise, it is false. 
    Requires: [p] has type [Piece.piece]. *)
val blockable_movement: piece -> bool

(** [rank_to_string] returns string version of [rank]. Ex. [rank_to_string 
    Pawn] is ["pawn"]. Requires: [rank] has type [Piece.rank]. *)
val rank_to_string: rank -> string

(** [string_to_rank string] is the rank option of [string]. Ex: [string_to_rank 
    "pawn"] is [Some Pawn]. Requires: [string] has type [string].  *)
val string_to_rank: string -> rank option

(** [column_from_location l] returns the character of location [l]
    Requires: l is a Piece.location
*)
val column_from_location: location -> char

(** [row_from_location l] returns the row int value of location [l]
    Requires: l is a Piece.location
*)
val row_from_location: location -> int

(** [equal piece1 piece2] is true if [piece1] and [piece2] has the same team,
    rank, location, and first_move and is false otherwise. Requires: [piece1] 
    and [piece2] has type [Piece.piece]. *)
val equal: piece -> piece -> bool

(** [okay_move start finish piece taking] is true if a [piece] that is or 
    is not going to take another piece ([taking] is true if [piece] is going to 
    take another piece, false otherwise) can move from [start] to [finish]
     and is false otherwise. Requires: [start] and [finish] has type [location], 
     [piece] has type [Piece.piece], and [taking] has type [bool].  *)
val okay_move: char * int -> char * int -> piece -> bool -> bool

