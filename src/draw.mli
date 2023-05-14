(** Module Draw draws the game borard and the representation of the cards to the
    users*)

val number_to_card : int -> int
(** [number_to_card x] convert x to the number on the card that is represented
    by x *)

val number_to_suit : int -> string
(** [number_to_suit x] convert x to the suit on the card that is represented by
    x *)

val print_card : int -> string
(** [print_card card_number] converts a card into a string format for better
    display *)

val print_cards : int list -> string
(** [print_cards cards] returns a string of cards to beat *)

val display_prev_cards : int list -> string
(** [display_prev_cards cards] returns a string to display cards to beat *)

val guide_player : string
(** [guide_player] is Guide on top of the board for player's turn*)

val guide_fst_player : string
(** [guide_fst_player] is Guide on top of the board for player's first turn*)

val guide_invalid_player : string
(** [guide_invalid_player] is Guide on top of the board for player's invalid
    turn*)

val indices_helper : int -> string
(** [indices_helper i ] convert i to string '| i|' or '|i|'*)

val indices_list : int -> string list
(** [indices_lst i ] returns lst ['|0|', '|1|', ..., '|i|']*)

val indices : int list -> string
(** [indices player_cards] returns the indice bar used for the game*)

val top_board : int list -> string
(** [top_board player_cards] is the Board Top*)

val bottom_board : int list -> string
(** [bottom_board player_cards] is the Board Bottom*)

val print_board : int list -> int list -> int list -> int list -> string
(** [print_board fst_ai_cards snd_ai_cards player_cards prev_cards] takes
    current cards state and print the board when it's player's turn*)

val print_fst_board : int list -> int list -> int list -> string
(** [print_fst_board fst_ai_cards snd_ai_cards player_cards] takes the beginning
    cards state and print the board*)

val print_invalid_board : int list -> int list -> int list -> int list -> string
(** [print_fst_board fst_ai_cards snd_ai_cards player_cards prev_cards] prints
    the board when the user has invalid input *)

val print_player_choice : int list -> string
(** [print_player_choice player_choice] outputs player's choice of cards to put
    down *)

val print_ai_choice : int list -> string
(** [print_ai_choice ai_choice] outputs the first ai's choice of cards to put
    down *)

val print_ai2_choice : int list -> string
(** [print_ai2_choice ai2_choice] outputs the second ai's choice of cards to put
    down *)
