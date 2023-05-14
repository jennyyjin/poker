(** Module Comparison implements the compare_card function and compare mutiples
    combinations of cards. Also checks the type of combinations.*)

exception Wrong
(** Raised when the pattern of the cards are not the desired pattern *)

type choice =
  | Continue of int list
  | Skip
  | Other

(** Type choice represents the operations of putting down the deck. [Continue n]
    represent you can continue by puttting down a certain deck. [Skip] means
    that you don't have a certain deck and you want to Skip. [Other] means that
    you dont have a deck of that pattern that is greater *)

type compare =
  | EQ
  | LT
  | GT
  | IV

(** Type compare represents the relative strength of certain deck [EQ] means the
    deck is equal. [LT] means the deck is smaller. [GT] means the deck if
    greater [IV] means not a valid form of deck*)

type cardstype =
  | Single
  | Double
  | Triple
  | TripleOne
  | Fullhouse
  | Straight
  | Bomb
  | Joker
  | Invalid
  | Empty

(** Type cardtypes represents the card types in our game of poker. [Single]
    means you have a single card of any rank. [Double] means you have two cards
    in the same rank.[Triple] means you have three cards in the same rank.
    [TripleOne] means you have three cards in the same rank with a [Single].
    [Fullhouse] means you have three cards in the same rank with a
    [Double].[Straight] means you have five cards in increasing rank regardless
    of suits. [Bomb] means you have four cards in the same rank. [Joker] means
    the two jokers. [Invalid] means you do not have a valid form of
    combinations. [Empty] means you have a empty deck.*)

val compare_card : int -> int -> int
(** [compare_card x y] is a comparison function that output either -1, 0 or 1 to
    denote the relative rank between cards. So we denote 0 and anything mod 13
    to 0 as the samllest rank of the deck, and 53 as the largest Joker in the
    deck *)

val sorted : int list -> int list
(** [sorted cards] sorts the list of cards in ascending order based on card rank *)

val getcardtype : int list -> cardstype
(**[getcardtype this] returns this's card type *)

val single : int list -> int list -> choice
(** [single this other] returns a single card to put down in response to the
    single card the other player just put down. Returns [Continue card] where
    card is the list of card to put down if there is a card in [this] with a
    greater rank than the opponent's card and [Other] if there are no cards
    greater. Requires: [this] is not empty, and [other] contains only one
    integer *)

val first_straight : int list -> int list -> choice
(** [first_straight this other] returns the first straight found on card list
    this *)

val straight : int list -> int list -> choice
(** [straight this other] returns a straight to put down in response to the
    straight the other player just put down. Returns [Continue card] where card
    is the list of cards to put down if there is a straight in [this] with
    greater ranks than [other] and [Other] if there are no cards greater.
    Requires: [this] is not empty, and [other] is a list of five consecutive
    numbers with no duplicates *)

val dup_list : int list -> int list
(** [dup_list lst] only contains duplicates *)

val double : int list -> int list -> choice
(** [double this other] returns [Continue card] where card is a pair of the same
    rank if there is a pair in [this] greater than [other] and [Other] otherwise *)

val three_lst : int list -> int list
(** [threes_lst lst] only contains triples *)

val triple : int list -> int list -> choice

(** [triple this other] returns [Continue card] where card is a triple of the
    same rank if there is a triple in [this] greater than [other] and [Other]
    otherwise *)

val find_quad : int list -> choice
(** [find_quad lst] returns [Continue card] where card is a list of four cards
    of the same rank if there is a four-of-a-kind in [lst] and [Other] otherwise *)

val quad : int list -> int list -> choice
(** [quad_lst] returns [Continue card] where card is a list of four cards of the
    same rank if there is a four-of-a-kind in [lst] and [Other] otherwise *)

val remove_joker : int list -> int list
(** [remove_joker lst] returns a list of cards with jokers removed *)

val joker_pair : int list -> choice
(** [joker_pair lst] returns [Continue card] where card is a list of two jokers
    if there are two jokers in [lst] and [Skip] otherwise *)

val triple_p_one : int list -> int list -> choice
(** [triple_p_one this other] returns [Continue card] where card is a list of
    four cards of the same rank if there is a one-of-a-kind in [lst] and [Other]
    otherwise *)

val triple_p_double : int list -> int list -> choice
(** [triple_p_double this other] returns [Continue card] where card is a list of
    four cards of the same rank if there is a two-of-a-kind in [lst] and [Other]
    otherwise *)

val check_same_type : int list -> int list -> bool
(**[check_same_type this other] returns true if this and that have the same card
   type *)

val compare_same_type : int list -> int list -> compare
(**[compare_same_type this other] returns true if this is greater than that
   Precondition: this and that have the same card type *)

val compare_diff_type : int list -> int list -> compare
(**[compare_diff_type this other] returns true if this is greater than that
   Precondition: this and that have different card types *)

val check_valid : int list -> int list -> bool
(**[check_valid card1 card2] returns true if c1 is greater than c2*)
