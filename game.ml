module MyRound = Pokergame.GameRound (Data.CardGame)
open MyRound


(* [play info] takes in [info] and plays the game until there is a winner, and
 * then this method returns that winner *)
let rec play info =
  let loser = play_round info in
  let new_info = update_state (loser) info in
  match winner new_info with
    | Some p -> p
    | None -> play new_info


(* [main n d] plays the game and then prints the winner out to the terminal
 * once a winner is determined *)
let main n d =
  let state = init_state n d in
  let w = play state in
  let col = if w = 1 then ANSITerminal.green
            else ANSITerminal.red in
  ANSITerminal.(print_string [col] ("Player "^(string_of_int (w mod 10))
                                  ^" wins!\n"))

