(** 
   Representation of dynamic adventure state.

   This module represents the state of an chess game as it is being played,
   including the pieces on the board, the current team, and if there is check
   or checkmate.
*)


(** The abstract type of values representing the state of the game. *)
type t 

exception Invalid_Move

(** [win_condition_black pie] is true iff the white king has been taken off the
    board by a move from the white team and is false otherwise. *) 
val win_condition_black : t -> bool

(** [win_condition_white pie] is true iff the white king has been taken off the
    board by a move from the black team and is false otherwise. *)
val win_condition_white : t -> bool

(** [init_state] is the initial state of the board. 
    In that state the current set up is the traditional chess start with it 
    being white's turn to move. *)
val init_state : t

(** [init_pieces] is a list of pieces in the initial setup of a tradtional game
    of chess.  *)
val init_pieces : Piece.piece list

(** [make_state t p] is the state with team [t] next to move, and all active 
    pieces on the board [p]. Requires: [t] has type [Piece.team] and [p] has
     type [Piece.piece list]. *)
val make_state : Piece.team -> Piece.piece list -> bool -> bool -> t

(** [contents s l] is the piece option of what is found at that location [l] in 
    state [s]. [contents s l] is [Some x] if there is a piece present at that
    location, and is [None] otherwise. Requires: [t] has type [State.t] and 
    [l] has type [location]. *)
val contents : t -> Piece.location -> Piece.piece option

(** [get_current_team t] is the string of the team who is allowed to move next. 
    Ex. If black is up next in [t], then [get_current_team t] is 
    ["Black team, "]. Requires: [t] has type [State.t]. *)
val get_current_team : t -> string

(** [move com t] is the state that results from executing [com] in state [t]. 
    Requires: [com] has type [Command.command] and [t] has type [State.t]. 
    Raises: [Invalid_Move] if the move is not possible or allowed in [t]. *)
val go : Command.command -> t -> t

(** [white_taken list] is the tuple containing the string of each of the rank 
    for the white team, and the number of pieces of the white team that have
     been taken by the black team in the list of active pieces [piece]. 
     Requires: [list] has type [Piece.piece list]. *)
val white_taken : Piece.piece list -> (string * int) list

(** [black_taken list] is the tuple containing the string of each of the rank 
    for the black team, and the number of pieces of the black team that have
     been taken by the white team in the list of active pieces [piece]. 
     Requires: [list] has type [Piece.piece list]. *)
val black_taken : Piece.piece list -> (string * int) list

(** [active_pieces t] is the [Piece.piece list] of all the active_pieces in 
    state [t]. Requires: [t] has type [State.t]. *)
val active_pieces : t -> Piece.piece list

(** [check_detection t] is true if the team up next to move in state [t] is in 
    check by the other team and false otherwise. Requires: [t] has type 
    [State.t]. *)
val check_detection : t -> bool

(** [checkmate_detection t] is true if the team that is up next to move in 
    state [t] is in checkmate by the other team and false otherwise. Requires: 
    [t] has type [State.t]. *)
val checkmate_detection : t -> bool

(** [white_score act_list] is a calculation of the score that the white team
    has. It is calculated as follows: black pawns add 1 point, black bishops and
    knights are each worth 3 points, black rooks are worth 5 points, and the 
    blackqueen is worth 9 points. Requires that [act_list] is a valid list of 
    pieces.*)
val white_score: Piece.piece list -> int

(** [black_score act_list] is a calculation of the score that the black team
    has. It is calculated as follows: white pawns add 1 point, white bishops and
    knights are each worth 3 points, white rooks are worth 5 points, and the 
    whitequeen is worth 9 points. Requires that [act_list] is a valid list of 
    pieces.*)
val black_score: Piece.piece list -> int
