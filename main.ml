(* [get_number_players ()] prompts the user for input to initialize the number
 * of players for the game. It will re-prompt the user to renter the number of
 * players if the user enters an invalid input *)
let rec get_number_players () =
  try (print_endline "Please enter the number of players.";
  print_string ">";
  let num_players = read_line () |> int_of_string in
  if (num_players < 2) then
    (print_endline ("The number of players must be at least 2. Please try "^
      "again.");
    get_number_players ())
  else if num_players > 9 then
    (print_endline "The number of players may be at most 9. Please try again.";
    get_number_players ())
  else num_players)
  with Failure _ -> ANSITerminal.(print_string [red]
                    ("The number of players must "^
                    "be an integer between 2 and 9. Please try "^
                    "again."));
                    get_number_players ()


(* [get_difficulty ()] prompts the user for input to initialize the difficulty
 * of the game. It will re-prompt the user to renter the difficulty level if
 * the user enters an invalid input *)
let rec get_difficulty () =
  try (print_endline ("Please enter the difficulty 1-3.");
  print_string ">";
  let diff = read_line () |> int_of_string in
  if (diff < 1) then
    (print_endline "The difficulty must be at least 1. Please try again.";
    get_difficulty ())
  else if diff > 3 then
    (print_endline "The difficulty may be at most 3. Please try again.";
    get_difficulty ())
  else diff)
  with Failure _ -> ANSITerminal.(print_string [red]
                    ("The difficulty must be an "^
                    "integer between 1 and 3. Please try again."));
                    get_difficulty ()


(* The method for initializing the game *)
let () =
  print_endline ("Welcome to Liar's Poker!");
  let num_players = get_number_players () in
  let diff = get_difficulty () in
  Game.main num_players diff