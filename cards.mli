module type Cards = sig
  (* The type of a rank of a card *)
  type rank

  (* The type of a suit of a card *)
  type suit

  (* The type of a card *)
  type card

  (* The type of a hand *)
  type hand

  (* The type of a deck *)
  type deck

  (* [empty] is the empty deck *)
  val empty : deck

  (* [print_hand h] prints a string representation of [h] to the terminal *)
  val print_hand : hand -> unit

  (* [new_deck e] takes in an empty deck [e] and returns a new, unshuffled 52
   * card deck with the cards in order *)
  val new_deck : deck -> deck

  (* [shuffle_deck d] takes in a deck [d] and returns a shuffled copy of the
   * original deck *)
  val shuffle_deck: deck -> deck

  (* [deal n d] creates a hand of the first [n] cards off of the top of deck d,
   * returning a hand with [n] cards *)
  val deal : int -> deck -> hand

  (* [string_of_rank r] is a string representation of rank [r] *)
  val string_of_rank : rank -> string

  (* [in_play hands] is the hand made up of all [hands] in play *)
  val in_play : hand list -> hand

  (* [string_of_card c] is a string representation of [c] *)
  val string_of_card : card -> string

end

module CardGame