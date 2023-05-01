val number_to_card : int -> int
(** [number_to_card card_number] takes the serial number of the card and return
    its number shown on the card. *)

val number_to_suit : int -> string
(** [number_to_suit card_number] takes the serial number of the card and return
    its suit shown on the card. *)

val print_card : int -> string
(** [print_card card_number] takes the serial number of the card and return the
    string for its drawing on board *)

val print_cards : int list -> string
(** [print_card card_number] takes a list of serial numbers of the cards and
    return the string for their drawing on board *)

val print_board : int list -> int list -> int list -> int list -> string
(** [print_board ] takes current cards state and print the board when it's
    player's turn*)

val print_ai_board : int list -> int list -> int list -> int list -> string
(** [print_ai_board ] takes current cards state and print the board when it's
    first AI's turn*)

val print_snd_ai_board : int list -> int list -> int list -> int list -> string
(** [print_snd_ai_board ] takes current cards state and print the board when
    it's second AI's turn*)
