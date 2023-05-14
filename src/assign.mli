(** Module Assign represent the assignments of cards in poker*)

val card_list : unit -> int list
(** [card_list ()] is a funciton that produce a list of integer from 0 to 53
    inclusive*)

val random : int -> int
(** [random n] reinitialize the ramdom number generator and generate a random
    number between 0 and n*)

val scrambled_list : unit -> int list
(** [scrambled_list ()] is the 54 cards in a poker deck and we name them from 0
    to 53 and scramble them up*)

val split_in_three : 'a list -> 'a list * 'a list * 'a list
(** [split_in_three lst] splits up the cards into three decks in which the first
    person gets the card if there is only one card. The first person and second
    person gets one card if there is two cards *)
