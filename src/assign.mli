type card =
  | Spade of int
  | Heart of int
  | Diamond of int
  | Club of int

val card_list : int list
val scrambled_list : int list
val split_in_half : 'a list -> 'a list * 'a list
