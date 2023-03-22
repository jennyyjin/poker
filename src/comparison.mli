exception Empty
exception Wrong

type choice =
  | Continue of int list
  | Skip
  | Other

val compare_card : int -> int -> int
val sorted : int list -> int list
val sorted_uniq : int list -> int list
val single : int list -> int list -> choice
val remove_joker : int list -> int list
val int_list_to_string : int list -> string
val char_to_int : char -> int
val straight : int list -> int list -> choice
val double_helper : int list -> int list
val double_lst : int list -> int list -> int list
val double : int list -> int list -> choice
val triple_helper : int list -> int list
val triple_lst : int list -> int list -> int list
val triple : int list -> int list -> choice
val quad : int list -> choice
val joker_pair : int list -> choice
val triple_p_double : int list -> int list -> choice
val play : int list -> int list -> choice
