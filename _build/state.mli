(** 
   Representation of dynamic adventure state.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.
*)

(* You are free to add more code here. *)

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The abstract type of values representing the pieces of the game. *)
type t 

exception Invalid_Move

(** win_condition_black pie is true iff the white king has been taken off the
    board by a move from the white*) (** ADD FORFIT OF THE OTHER TEAM LEADING TO A WIN???*)
val win_condition_black : t -> bool

(** win_condition_white pie is true iff the white king has been taken off the
    board by a move from the black*)
val win_condition_white : t -> bool


(** [init_state] is the initial state of the board. 
    In that state the current set up is the traditional chess start with it 
    being white's turn to move.*)
val init_state : t


val contents : t -> Piece.location -> Piece.piece option

val get_current_team : t -> string

(** move [com] [pie] with give back a State.t in which the command has been 
    exicuted.
    Throws an Invalid_Move exeption if the move is not possible.*)
val go : Command.command -> t -> t


(* You are free to add more code here. *)