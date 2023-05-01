type card =
  | Spade of int
  | Heart of int
  | Diamond of int
  | Club of int

val card_list : int list
(** [card_list] is the 54 cards in a poker deck and we name them from 1 to 54 *)

val random : int -> int
(**[random n] generate a randome integer with range 0(inclusive) to n(exclusive) *)

val shuffling : int list -> int list -> int list
(** [shuffling] simulates shuffling cards in poker*)

val scrambled_list : int list
(** [scrambled_list] is the 54 cards in a poker deck and we name them from 1 to
    54 and scramble them up*)

val split_in_half : 'a list -> 'a list * 'a list
(** [split_in_half lst] splits up the cards into two decks.*)

val split_in_three : 'a list -> 'a list * 'a list * 'a list
(** [split_in_half lst] splits up the cards into three decks, the first two
    decks each have 17 cards while the last one have 20 cards*)
