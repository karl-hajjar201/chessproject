(** 
   This is the module that uses a State.t to print the board.

   This module uses State.t value passed into it to print out the board in the
   terminal. it uses Ansi.terminal to apply coloring and style to the board
*)

(**Type representing the board *)
type b

(** [print_board t] prints the board in the terminal given state [t]. 
    Requires: [t] has type [State.t]. *)
val print_board : State.t -> unit
