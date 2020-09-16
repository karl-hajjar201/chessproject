open Adventure
open State
open Command
open Piece
open Board

(** [two_player_game b] starts the two player game. with the piece list t*)
let rec two_player_game t =
  Board.print_board t;
  print_endline (State.get_current_team t ^ "type your move (ex:  King h4 h5) or forfit");
  print_string "> ";
  match Command.parse (read_line ()) with
  | Forfit -> print_endline "Thanks for playing!"; exit 0
  | command -> two_player_game (State.go command t)
  | exception Malformed -> print_endline "This command is invalid. Please try again."; two_player_game t
  | exception End_of_file -> ()

(* will have a call here to parse. it will match the result of it with a command for go. *)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [yellow] 
                  "\n\nWelcome to THE BOYZ Chess Game. \n");
  print_endline "Please type begin to begin the game.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "begin" -> two_player_game State.init_state
  | _ -> print_string("Sorry try again. Soon we will impliment other options")

(* Execute the game engine. *)
let () = main ()
