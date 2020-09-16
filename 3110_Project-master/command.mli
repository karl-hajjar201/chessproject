
(** 
   Command represents the possible commands to be issued by the player.

   Commands cound include moving a piece, asking for the score, or forfiting the 
   game.
*)

(** The type representation when looking to move a piece from one location
    to another.*)
type move_set = Piece.location * Piece.location

(** The type [command] represents a player command that is decomposed
    into a Piece and location. *)
type command = 
  |Pawn of move_set
  |Knight of move_set
  |Rook of move_set
  |King of move_set
  |Queen of move_set
  |Bishop of move_set
  |Forfeit
  |Taken
  |Score

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(**[make_com s ms] makes a command representation of the given string s and 
   move_set ms
   Requires: s is a string and ms is a valid move_set*)
val make_com: string -> move_set -> command

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the move set.
    Examples: 
    - [parse "    pawn  e2 e3  "] is [Pawn (('e',2),('e',3))]
    - [parse "forfit"] is [Forfit] and ect. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed.*)
val parse : string -> command


(**[first_from_moveset ms] returns the first location from move_set ms.
   Requires: ms is a valid moveset
*)
val first_from_moveset: move_set -> Piece.location

(**[second_from_moveset ms] returns the second location from move_set ms.
   Requires: ms is a valid moveset.
*)
val second_from_moveset: move_set -> Piece.location
