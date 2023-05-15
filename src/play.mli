(**Module Play contains methods for deck operations.*)

val find_index : int -> int list -> int -> int
(** [find_index element lst index] returns the index of the element in the list *)

val check_not_index : int -> int list -> int list -> bool
(** [check_not_index e c i] checks to see if the list is in index*)

val update_cards : int list -> int list -> int list
(** [update_cards current_cards output_cards] returns the rest of the cards when
    some cards are placed down。 This is for player. *)

val check_not_card : int list -> int -> bool
(** [check_not_card lst card] delete card from lst *)

val update_ai_cards : int list -> int list -> int list
(** [update_ai_cards current_cards output_cards] returns the rest of the cards
    when some cards are placed down. This is for AIs *)

val index_to_num : int list -> int list -> int list
(** [index_to_num current_cards output_cards ] returns the list of cards that is
    placed down by the player based on players index input *)

val clear : int -> int
(**[clear] clear the screen*)
