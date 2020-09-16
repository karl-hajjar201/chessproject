open OUnit2
open Command
open State
open Piece
open Board

(** 
   This is the testing module.

   This module tests various functions. our methodology folows below
*)

(**
   TESTING METHODOLOGY:
   This test suite tests mainly the base functions needed for the foundation of the
   system. Then once getting onto more advanced functions, those were tested 
   manually using the system interface. This was done because the larger 
   functions need many variables and cause a hassle when testing automatically.
    The more mainfunctions were tested manually as well since when playing the 
    game, the interface is the only interation point with the system so if 
    everything works there, then there should be no issues the users will run 
    into. 

   When doing the automatic testing, we used black box testing which added the 
   benifit of even if someone didn't write the function, they got indepth
   experience implimenting it here to learn what it needs and outputs exactly 
   before running into comprehension issues later down the line when 
   implementing somewhere else.

   This overall approach to testing demonstrates the correctness of the system 
   since we did extensive tests using the interface as to ensure no bugs were
   noticable in the main game. Even though not every function was tested
   in this test suite, all of them were tested through the interface making us 
   confident there are no apparent bugs in how the system opperates.
   One key element was how we tested our checkmate detection. we did this by 
   playing a game which gave us the quickest route to checkmate. 
   This allowed us the test this then also trying a few other scenarios.
    This is because testing this function takes a lot of code since it 
    requires alot in the game state to check properly so it was simpler to run 
    various cases ingame.


*)


let com1 = make_com "pawn" (('e',3),('e',4))
let com2 = make_com "king" (('e',3),('e',4))
let com3 = make_com "queen" (('e',3),('e',4))
let com4 = make_com "knight" (('e',3),('e',4))
let com5 = make_com "bishop" (('e',3),('e',4))
let com6 = make_com "rook" (('e',3),('e',4))

(** Collection of command tests*)
let command_tests =
  [
    "testing normal valid commands, pawn" >:: 
    (fun _ -> assert_equal com1 (parse "pawn e3 e4"));
    "testing normal valid commands, king" >:: 
    (fun _ -> assert_equal com2 (parse "king e3 e4"));
    "testing normal valid commands, queen" >:: 
    (fun _ -> assert_equal com3 (parse "queen e3 e4"));
    "testing normal valid commands, bishop" >:: 
    (fun _ -> assert_equal com5 (parse "bishop e3 e4"));
    "testing normal valid commands, rook" >:: 
    (fun _ -> assert_equal com6 (parse "rook e3 e4"));
    "testing normal valid commands, knight" >:: 
    (fun _ -> assert_equal com4 (parse "knight e3 e4"));

    "Score commands 1" >:: 
    (fun _ -> assert_equal Score (parse "score"));
    "Score commands 2" >:: 
    (fun _ -> assert_equal Score (parse "scoRe "));
    "Score commands 3" >:: 
    (fun _ -> assert_equal Score (parse "  score   "));
    "Score commands 4" >:: 
    (fun _ -> assert_equal Score (parse "Score"));
    "Forfit commands 1" >:: 
    (fun _ -> assert_equal Forfeit (parse "forfeit"));
    "Forfit commands 2" >:: 
    (fun _ -> assert_equal Forfeit (parse "   forfeit  "));
    "Forfit commands 3" >:: 
    (fun _ -> assert_equal Forfeit (parse "FORFEIT"));
    "Taken commands 1" >:: 
    (fun _ -> assert_equal Taken (parse "taken"));
    "Taken commands 2" >:: 
    (fun _ -> assert_equal Taken (parse "   taken  "));
    "Taken commands 3" >:: 
    (fun _ -> assert_equal Taken (parse "tAKeN"));


    "testing normal valid commands, pawn, 
    however looking to test the upercase handeling" >:: 
    (fun _ -> assert_equal com1 (parse "PAWN e3 e4"));
    "testing normal valid commands, pawn, 
    however looking to test the upercase handeling" >:: 
    (fun _ -> assert_equal com1 (parse "pawn E3 e4"));

    "testing normal valid commands, pawn, 
    however looking to test theextra space handeling" >:: 
    (fun _ -> assert_equal com1 (parse "   pawn E3 e4"));
    "testing normal valid commands, pawn, 
    however looking to test theextra space handeling" >:: 
    (fun _ -> assert_equal com1 (parse "pawn    E3 e4"));
    "testing normal valid commands, pawn, 
    however looking to test theextra space handeling" >:: 
    (fun _ -> assert_equal com1 (parse "pawn E3    e4"));
    "testing normal valid commands, pawn, 
    however looking to test theextra space handeling" >:: 
    (fun _ -> assert_equal com1 (parse "pawn E3 e4   "));


    "Testing first from move_set" >:: 
    (fun _ -> assert_equal ('e',2) (first_from_moveset (('e',2),('e',3))));

    "Testing second from move_set" >:: 
    (fun _ -> assert_equal ('e',3) (second_from_moveset (('e',2),('e',3))));
  ]


let pawn_test = set_piece ('e',2) Pawn Black true
let pawn_test1 = set_piece ('e',2) Pawn Black true
let rook_test = set_piece ('e',2) Rook Black true
let king_test = set_piece ('e',2) King Black true
let queen_test = set_piece ('e',2) Queen Black true
let bishop_test = set_piece ('e',2) Bishop Black true
let knight_test = set_piece ('e',2) Knight Black true
let knight_test' = set_piece ('e',2) Knight White true

(** Collection of piece tests*)
let piece_tests =
  [
    "testing set piece of pawns" >:: 
    (fun _ -> assert_equal "Black pawn at e2, true" 
        (string_of_piece (pawn_test)));

    "testing set piece of rook" >:: 
    (fun _ -> assert_equal "Black rook at e2, true" 
        (string_of_piece (rook_test)));

    "testing set piece of Knight" >:: 
    (fun _ -> assert_equal "Black knight at e2, true" 
        (string_of_piece (knight_test)));

    "testing set piece of Bishop" >:: 
    (fun _ -> assert_equal "Black bishop at e2, true" 
        (string_of_piece (bishop_test)));

    "testing set piece of King" >:: 
    (fun _ -> assert_equal "Black king at e2, true" 
        (string_of_piece (king_test)));

    "testing set piece of Queen" >:: 
    (fun _ -> assert_equal "Black queen at e2, true" 
        (string_of_piece (queen_test)));

    "Testing Get team Black" >::
    (fun _ -> assert_equal Black (get_team pawn_test));

    "Testing get team White" >::
    (fun _ -> assert_equal White (get_team knight_test'));


    "Testing get rank pawn" >::
    (fun _ -> assert_equal Pawn (get_rank pawn_test));

    "Testing get rank king" >::
    (fun _ -> assert_equal King (get_rank king_test));

    "Testing get rank Queen" >::
    (fun _ -> assert_equal Queen (get_rank queen_test));

    "Testing get rank Bishop" >::
    (fun _ -> assert_equal Bishop (get_rank bishop_test));

    "Testing get rank Rook" >::
    (fun _ -> assert_equal Rook (get_rank rook_test));

    "Testing get rank kngiht" >::
    (fun _ -> assert_equal Knight (get_rank knight_test));



    "Blockable Movement Test queen" >:: 
    (fun _ -> assert_equal true 
        (blockable_movement queen_test));

    "Blockable Movement Test bishop" >:: 
    (fun _ -> assert_equal true 
        (blockable_movement bishop_test));

    "Blockable Movement Test room" >:: 
    (fun _ -> assert_equal true 
        (blockable_movement rook_test));

    "Blockable Movement Test false pawn" >:: 
    (fun _ -> assert_equal false
        (blockable_movement (pawn_test)));

    "Blockable Movement Test false king" >:: 
    (fun _ -> assert_equal false
        (blockable_movement (king_test)));

    "Blockable Movement Test false knight" >:: 
    (fun _ -> assert_equal false
        (blockable_movement (knight_test)));


    "Getting Column Test" >:: 
    (fun _ -> assert_equal 'e' (column_from_location ('e', 3)));

    "Getting Row Test" >:: 
    (fun _ -> assert_equal 3 (row_from_location ('e', 3)));


    "True Testing Piece Equality" >:: 
    (fun _ -> assert_equal true
        (equal pawn_test pawn_test1));

    "False Testing Piece Equality" >:: 
    (fun _ -> assert_equal false
        (equal pawn_test queen_test));

  ]

let p1 = set_piece ('e',2) Pawn Black true
let p1' = set_piece ('e',3) Pawn Black true
let p2 = set_piece ('f',2) Pawn Black true
let p3 = set_piece ('g',2) Pawn Black true
let king1 = set_piece ('e',1) King Black true
let knight1 = set_piece ('d',1) Knight Black true
let q1 = set_piece ('a',1) Queen Black true
let r1 = set_piece ('b',1) Rook Black true
let b1 = set_piece ('c',1) Bishop Black true

let wp1 = set_piece ('e',7) Pawn White true
let wp2 = set_piece ('f',7) Pawn White true
let wp3 = set_piece ('g',7) Pawn White true
let wking1 = set_piece ('e',8) King White true
let wknight1 = set_piece ('d',8) Knight White true
let wq1 = set_piece ('a',8) Queen White true
let wr1 = set_piece ('b',8) Rook White true
let wb1 = set_piece ('c',8) Bishop White true

(** Check setting up the board. Later used to test checkmate detection?*)
let p1_check = set_piece ('e',2) Pawn Black true
let p2_check = set_piece ('f',2) Pawn Black true
let p3_check = set_piece ('g',2) Pawn Black true
let king1_check = set_piece ('e',1) King Black true
let knight1_check = set_piece ('d',1) Knight Black true
let q1_check = set_piece ('a',1) Queen Black true
let r1_check = set_piece ('b',1) Rook Black true
let b1_check = set_piece ('b',4) Bishop Black true

let wp1_check = set_piece ('e',5) Pawn White true
let wp2_check = set_piece ('f',4) Pawn White true
let wp3_check = set_piece ('g',6) Pawn White true
let wking1_check = set_piece ('e',8) King White true
let wknight1_check = set_piece ('d',8) Knight White true
let wknight1_check' = set_piece ('d',2) Knight White true
let wq1_check = set_piece ('a',8) Queen White true
let wr1_check = set_piece ('b',8) Rook White true
let wb1_check = set_piece ('c',8) Bishop White true

let game_1 = [p1;p2;p3;knight1;q1;r1;b1;
              wp1;wp2;wp3;wking1;wknight1;wq1;wr1;wb1]

let state_1 = make_state White game_1

let game_2 = [p1;p2;p3;knight1;king1;q1;r1;b1;
              wp1;wp2;wp3;wknight1;wq1;wr1;wb1]

let state_2 = make_state White game_2

let game_check = [p1_check;p2_check;p3_check;knight1_check;q1_check;
                  r1_check;b1_check;wp1_check;wp2_check;wp3_check;
                  wking1_check;wknight1_check;wq1_check;wr1_check;wb1_check]
let state_check = (make_state White game_check) true false
let game_check' = [p1_check;p2_check;p3_check;knight1_check;q1_check;
                   r1_check;b1_check;wp1_check;wp2_check;wp3_check;
                   wking1_check;wknight1_check';wq1_check;wr1_check;wb1_check]
let state_check' = (make_state White game_check') false false

(** Collection of state tests*)
let state_tests =
  [
    "Win Condition Black true" >:: 
    (fun _ -> assert_equal true (win_condition_black(state_2 false false)));

    "Win Condition Black false" >:: 
    (fun _ -> assert_equal false (win_condition_black(state_1 false false)));

    "Win Condition White false" >:: 
    (fun _ -> assert_equal false (win_condition_white(state_2 false false)));

    "Win Condition White true" >:: 
    (fun _ -> assert_equal true (win_condition_white(state_1 false false)));


    "Contents Testing None" >:: 
    (fun _ -> assert_equal None (contents (state_1 false false) ('g', 5)));

    "Contents Testing and Actual piece" >:: 
    (fun _ -> assert_equal (Some p1) (contents (state_1 false false) ('e', 2)));

    "Testing Check 1" >:: 
    (fun _ -> assert_equal false (check_detection state_check'));

    "Testing Check 2" >:: 
    (fun _ -> assert_equal false (check_detection state_check));



  ]


let board_tests =
  [
    (*No testcases needed. only visual testing in the terminal*)
  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    command_tests;
    state_tests;
    piece_tests;
    board_tests;
  ]

let _ = run_test_tt_main suite
