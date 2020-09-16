open State
open Command
open Piece
open Board

(** Helper function to check for check and check mate in State.t [t]
    Requires: t is a State.t*)
let helper_check_and_checkmate t =
  if checkmate_detection t 
  then ANSITerminal.(print_string [red] ((State.get_current_team t) 
                                         ^ " you're in checkmate, mate. \n")) 
  else if check_detection t 
  then ANSITerminal.(print_string [red] ((State.get_current_team t) 
                                         ^ " you're in check, mate. \n")) 
  else ()

let score t = ANSITerminal.(print_string [blue] "**WHITE SCORE**   ");
  print_int (State.white_score (State.active_pieces t));
  print_endline("\n");
  ANSITerminal.(print_string [red] "**BLACK SCORE**   ");
  print_int (State.black_score (State.active_pieces t))

let taken t = let white = State.white_taken (State.active_pieces t) in
  let black = State.black_taken (State.active_pieces t) in
  ANSITerminal.(print_string [blue] "**WHITE TEAM PIECES LOST**\n"); 
  List.iter (fun (x,y)-> print_string x; print_string ": "; 
              print_int y; print_string "| ") white; print_string "\n";
  ANSITerminal.(print_string [blue] "**BLACK TEAM PIECES LOST**\n"); 
  List.iter (fun (x,y)-> print_string x; print_string ": "; 
              print_int y; print_string "| ") black; print_string "\n"

(** [two_player_game b] starts the two player game. with the State t
    Requires: t is a State.t. *)
let rec two_player_game t = 
  if win_condition_white t then (print_endline "White team, you won!."; exit 0)
  else ();
  if win_condition_black t then (print_endline "Black team, you won!."; exit 0)
  else ();
  Board.print_board t; helper_check_and_checkmate t;
  print_endline (State.get_current_team t 
                 ^ "type your move (ex:  King h4 h5), forfeit, score, or taken");
  print_string "> ";
  match Command.parse (read_line ()) with
  | Forfeit -> print_endline "Thanks for playing!"; exit 0
  | Score -> score t; two_player_game t
  | Taken -> taken t; two_player_game t
  | command -> begin try two_player_game (State.go command t) 
      with Invalid_Move -> 
        print_endline "Something is wrong with your move. Please try again"; 
        two_player_game t end
  | exception Malformed -> print_endline "Invalid Command"; two_player_game t
  | exception End_of_file -> ()

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
