val find_index : int -> int list -> int -> int
(** [find_index] takes one element from a list, a list and a start index number,
    it runs recursively find the index of the element in the list by starting
    with the start index*)

val check_not_index : int -> int list -> int list -> bool
(** [check_not_index] takes one element from a list, a list and a list of
    indiced number, it returns true if the element's index is on the index_lst,
    false otherwise*)

val update_cards : int list -> int list -> int list
(** [update_cards] takes a serial card list that player holds and the cards
    player placed, and update the card list.*)

val check_not_card : int list -> int -> bool
(** [check_not_index] takes a list and a card serial number, it returns true the
    serial number is not on the list*)

val update_ai_cards : int list -> int list -> int list
(** [update_cards] takes a serial card list that player holds and the cards
    player placed, and update the card list.*)

val index_to_num : int list -> int list -> int list
(** [index_to_num] ... emmmmm forgot what it does...*)

val clear : int -> int
