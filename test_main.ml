open OUnit2
open Game
open Pokergame
open Data
open CardGame


let card_tests = [
  "format_face_card" >:: (fun _ -> assert_equal "Ace of Hearts"
    (string_of_card (14, Hearts)));
  "format_non_face_card" >:: (fun _ -> assert_equal "2 of Spades"
    (string_of_card (2, Spades)));
]


let empty_deck = empty
let shuffled_deck =
  ref [(6, Spades); (5, Diamonds); (12, Diamonds); (13, Diamonds); (4, Clubs);
  (9, Clubs); (7, Clubs); (6, Clubs); (8, Clubs); (2, Diamonds);
  (4, Spades); (14, Spades); (7, Diamonds); (8, Hearts); (8, Diamonds);
  (5, Spades); (2, Clubs); (7, Spades); (12, Clubs); (9, Hearts);
  (4, Hearts); (3, Diamonds); (2, Hearts); (14, Clubs); (7, Hearts);
  (6, Hearts); (10, Spades); (11, Diamonds); (14, Diamonds); (13, Clubs);
  (8, Spades); (5, Clubs); (10, Clubs); (3, Spades); (11, Hearts);
  (9, Diamonds); (2, Spades); (11, Spades); (12, Spades); (3, Hearts);
  (12, Hearts); (11, Clubs); (10, Hearts); (13, Spades); (6, Diamonds);
  (10, Diamonds); (14, Hearts); (5, Hearts); (3, Clubs); (13, Hearts);
  (4, Diamonds); (9, Spades)]
  (* shuffled_deck created from shuffling a deck in utop *)

let deck_tests = [
  "empty_deck" >:: (fun _ -> assert_equal empty (empty_deck));
  "instantiate_new_deck" >:: (fun _ -> assert_equal
   (ref [(14, Diamonds); (13, Diamonds); (12, Diamonds); (11, Diamonds);
   (10, Diamonds); (9, Diamonds); (8, Diamonds); (7, Diamonds); (6, Diamonds);
   (5, Diamonds); (4, Diamonds); (3, Diamonds); (2, Diamonds); (14, Clubs);
   (13, Clubs); (12, Clubs); (11, Clubs); (10, Clubs); (9, Clubs); (8, Clubs);
   (7, Clubs); (6, Clubs); (5, Clubs); (4, Clubs); (3, Clubs); (2, Clubs);
   (14, Spades); (13, Spades); (12, Spades); (11, Spades); (10, Spades);
   (9, Spades); (8, Spades); (7, Spades); (6, Spades); (5, Spades);
   (4, Spades); (3, Spades); (2, Spades); (14, Hearts); (13, Hearts);
   (12, Hearts); (11, Hearts); (10, Hearts); (9, Hearts); (8, Hearts);
   (7, Hearts); (6, Hearts); (5, Hearts); (4, Hearts); (3, Hearts);
   (2, Hearts)]) (new_deck empty_deck));
  "deal_from_shuffled_deck" >:: (fun _ -> assert_equal
   ([(6, Spades); (5, Diamonds); (12, Diamonds); (13, Diamonds)])
   (deal 4 shuffled_deck));
  "check_deck_is_modified_after_deal" >:: (fun _ -> assert_equal
   [(4, Clubs); (9, Clubs); (7, Clubs); (6, Clubs); (8, Clubs); (2, Diamonds);
   (4, Spades); (14, Spades); (7, Diamonds); (8, Hearts); (8, Diamonds);
   (5, Spades); (2, Clubs); (7, Spades); (12, Clubs); (9, Hearts);
   (4, Hearts); (3, Diamonds); (2, Hearts); (14, Clubs); (7, Hearts);
   (6, Hearts); (10, Spades); (11, Diamonds); (14, Diamonds); (13, Clubs);
   (8, Spades); (5, Clubs); (10, Clubs); (3, Spades); (11, Hearts);
   (9, Diamonds); (2, Spades); (11, Spades); (12, Spades); (3, Hearts);
   (12, Hearts); (11, Clubs); (10, Hearts); (13, Spades); (6, Diamonds);
   (10, Diamonds); (14, Hearts); (5, Hearts); (3, Clubs); (13, Hearts);
   (4, Diamonds); (9, Spades)] (deal 4 shuffled_deck; !shuffled_deck));
]


let players_1 = [(1, 4); (2, 3); (3, 1); (4, 4)]
let players_2 = [(1, 1); (3, 1); (4, 3)]
let players_3 = [(1, 1); (4, 1)]
let players_4 = [(1, 3); (2, 4); (4, 1)]

let round_tests = [
  "player_1_loses" >:: (fun _ -> assert_equal
  [(1, 3); (2, 3); (3, 1); (4, 4)] (update_players 1 players_1 []));
  "player_2_loses" >:: (fun _ -> assert_equal
  [(1, 4); (2, 2); (3, 1); (4, 4)] (update_players 2 players_1 []));
  "player_3_is_out" >:: (fun _ -> assert_equal
  [(1, 4); (2, 3); (4, 4)] (update_players 3 players_1 []));
  "player_4_loses" >:: (fun _ -> assert_equal
  [(1, 4); (2, 3); (3, 1); (4, 3)] (update_players 4 players_1 []));
  "player_1_is_out_config_2" >:: (fun _ -> assert_equal
  [(3, 1); (4, 3)] (update_players 1 players_2 []));
  "player_3_is_out_config_2" >:: (fun _ -> assert_equal
  [(1, 1); (4, 3)] (update_players 3 players_2 []));
  "player_4_loses_config_2" >:: (fun _ -> assert_equal
  [(1, 1); (3, 1); (4, 2)] (update_players 4 players_2 []));
  "player_1_is_out_config_3" >:: (fun _ -> assert_equal
  [(4, 1)] (update_players 1 players_3 []));
  "player_4_is_out_config_3" >:: (fun _ -> assert_equal
  [(1, 1)] (update_players 4 players_3 []));
  "player_1_out_next_player" >:: (fun _ -> assert_equal 4
  (next_player 1 [(1, 1); (4, 1)]));
  "player_3_out_next_player" >:: (fun _ -> assert_equal 4
  (next_player 3 [(1, 4); (2, 3); (3, 1); (4, 4)]));
  "player_4_out_next_player_wrap_around" >:: (fun _ -> assert_equal 1
  (next_player 4 [(1, 3); (2, 4); (4, 1)]));
]

let suite = "Final Project test suite" >:::
  card_tests@deck_tests@round_tests

(* The following line must be the one and only place
 * in your entire source code that calls [OUnit2.run_test_tt_main]. *)
let _ = run_test_tt_main suite