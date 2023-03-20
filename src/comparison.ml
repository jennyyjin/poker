open Assign_card

exception Empty
exception InvaidDouble of int * int
exception InvaidForm

type choice =
  | Continue of int list
  | Skip

(** [compare_card x y] compares two cards x and y, returns 1 if card x has a
    higher rank than the card y, returns -1 if x has a smaller rank than y,
    returns 0 if they have the same rank. Note the jokers are the greatest *)
let compare_card x y =
  if x = 54 then 1
  else if y = 54 then -1
  else if x = 53 then 1
  else if y = 53 then -1
  else
    let x_num = x mod 13 in
    let y_num = y mod 13 in
    if x_num < y_num then -1 else if x_num > y_num then 1 else 0

(** [sorted cards] sorts the list of cards in ascending order based on card rank *)
let sorted (cards : int list) : int list = List.sort compare_card cards

(** [sorted cards] sorts the list of cards in ascending order based on card rank
    and removes duplicates *)
let sorted_uniq (cards : int list) : int list =
  List.sort_uniq compare_card cards

(** [single this other] returns a single card to put down in response to the
    single card the other player just put down, returns [Continue card] where
    card is the list of card to put down if there is a card in [this] with a
    greater rank than the opponent's card, returns [Skip] if there are no cards
    greater. Precondition: [this] is not empty, [other] contains only one
    integer *)
let rec single (this : int list) (other : int list) : choice =
  match this with
  | [] -> Skip
  | h :: t ->
      if compare_card h (List.hd other) = 1 then Continue [ h ]
      else single t other

(** [remove_joker lst] returns a list of cards with jokers removed *)
let rec remove_joker (lst : int list) =
  match lst with
  | [] -> []
  | h :: t -> if h = 53 || h = 54 then remove_joker t else h :: remove_joker t

(** [int_list_to_string lst] converts a list of integers to a string in
    ascending order without duplicates *)
let int_list_to_string (lst : int list) : string =
  let str_list = List.map string_of_int (sorted_uniq lst) in
  String.concat "" str_list

(** [char_to_int c] converts a char to an int *)
let char_to_int (c : char) : int = int_of_char c - int_of_char '0'

(** [straight this other] returns a straight to put down in response to the
    straight the other player just put down, returns [Continue card] where card
    is the list of cards to put down if there is a straight in [this] with
    greater ranks than the opponent's straight, returns [Skip] if there are no
    cards greater. Precondition: [this] is not empty, [other] is a list of five
    consecutive numbers with no duplicates *)
let rec straight (this : int list) (other : int list) : choice =
  match sorted_uniq (remove_joker this) with
  | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] | [ _; _; _; _ ] -> Skip
  | c1 :: c2 :: c3 :: c4 :: c5 :: t ->
      if
        (c1 mod 13) - (char_to_int (int_list_to_string other).[0] mod 13)
        = (c2 mod 13) - (char_to_int (int_list_to_string other).[1] mod 13)
        && (c2 mod 13) - (char_to_int (int_list_to_string other).[1] mod 13)
           = (c3 mod 13) - (char_to_int (int_list_to_string other).[2] mod 13)
        && (c3 mod 13) - (char_to_int (int_list_to_string other).[2] mod 13)
           = (c4 mod 13) - (char_to_int (int_list_to_string other).[3] mod 13)
        && (c4 mod 13) - (char_to_int (int_list_to_string other).[3] mod 13)
           = (c5 mod 13) - (char_to_int (int_list_to_string other).[4] mod 13)
      then Continue [ c1; c2; c3; c4; c5 ]
      else straight (c2 :: c3 :: c4 :: c5 :: t) other
