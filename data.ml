open String
open List

module type Cards = sig
  type rank = int

  type suit = Hearts | Clubs | Diamonds | Spades

  type card = (rank * suit)

  type hand = card list

  type deck = card list ref

  val empty : deck
  val print_hand : hand -> unit
  val new_deck : deck -> deck
  val shuffle_deck: deck -> deck
  val deal : int -> deck -> hand
  val string_of_rank : int -> string
  val in_play : hand list -> hand
  val string_of_card : card -> string
end

module CardGame : Cards = struct
  type rank = int

  type suit = Hearts | Clubs | Diamonds | Spades

  type card = (rank * suit)

  type hand = card list

  type deck = card list ref

  let empty = ref []

  (* [string_of_suit s] returns the string representation of suit [s]
   * requires: [s] is of type suit *)
  let string_of_suit (s:suit) =
    match s with
    | Spades -> "Spades"
    | Clubs -> "Clubs"
    | Diamonds -> "Diamonds"
    | Hearts -> "Hearts"


  (* [string_of_rank r] returns the string representation of rank [r]
   * requires: [r] is of type rank *)
  let string_of_rank (r:rank) =
    if r = 11 then "Jack"
    else if r = 12 then "Queen"
    else if r = 13 then "King"
    else if r = 14 then "Ace"
    else string_of_int r


  (* [string_of_card c] returns a string representation of a card [c]
   * requires: [c] is a valid card *)
  let string_of_card (c:card) : string =
    string_of_rank (fst c) ^ " of " ^ string_of_suit (snd c)


  let rec print_hand (h:hand) =
    match h with
    | [] -> ()
    | h::t -> print_endline (string_of_card h);
              print_hand t

  (* Helper function taken from Recitation 3: Lists, and Testing with OUnit
   * that creates an infix operator that makes a list of all integers from i
   * through j inclusive *)
  let (--) i j =
    let rec from i j l =
      if i>j then l
      else from i (j-1) (j::l)
      in from i j []


  (* [deck_helper suit rank_list accum] is a helper function to instantiate a
   * 52 card deck that returns all the cards for a particular suit, from 2
   * through ace (represented as rank 14)
   * [suit] is a suit
   * [rank_list] is a list of all possible ranks
   * [accum] is an accumulator holding the deck created so far *)
  let rec deck_helper suit rank_list accum =
    match rank_list with
    | [] -> accum
    | h::t -> deck_helper suit t ((h, suit)::accum)


  (* [suit_helper suit_list accum] is a helper function that for each suit in
   * [suit_list] calls [deck_helper] to instantiate cards 1-14 for it
   * [suit_list] is a list of all four possible suits
   * [accum] holds the deck created so far *)
  let rec suit_helper suit_list accum = match suit_list with
    | [] -> accum
    | h::t -> suit_helper t ((deck_helper h (2--14) [])@accum)


  let rec new_deck (e : deck) : deck =
    ref (suit_helper [Hearts; Spades; Clubs; Diamonds] (!e))


  (* [drop n lst] is a helper method for modifying the deck. It returns all but
   * the first [n] elements of [lst]. If [lst] has fewer than [n] elements,
   * return the empty list. Code taken from Recitation: Lists, and Testing with
   * OUnit *)
  let rec drop n lst =
    if n =0 then lst else match lst with
      | []->[]
      | x::xs -> drop (n-1) xs


  (* [take n lst] returns the first [n] elements of [lst]. If [lst] has fewer
   * than [n] elements, return all of them. Code taken from Recitation: Lists,
   * and Testing with OUnit *)
  let rec take n lst =
    if n = 0 then [] else match lst with
      | [] -> []
      | (x::xs) -> x :: (take (n-1) xs)


  let shuffle_deck (d : deck) : deck =
    let deck = !d in
    let array_deck = Array.of_list deck in
    Random.self_init ();
    for i=0 to 10000 do
      let first = Random.int 52 in
      let second = Random.int 52 in
      let old_card_one = array_deck.(first) in
      let old_card_two = array_deck.(second) in
      array_deck.(second) <- old_card_one;
      array_deck.(first) <- old_card_two;
    done;
    d := (Array.to_list array_deck);
    d


  let deal (n : int) (d : deck) : hand =
    let hand = take n !d in
    d := drop n !d;
    hand

  let in_play hands =
    List.flatten hands

  (*[get_rand_num] returns random number between lower bound [l] (incl) and
   *upper bout [u] (excl)*)
  let rec get_rand_num l u =
    Random.self_init ();
    let num = Random.int u in
    if num >= l then num else get_rand_num l u

  let random_card =
    (get_rand_num 2 15, Hearts)

  let num_cards hand =
    length hand
end