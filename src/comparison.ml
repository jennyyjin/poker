open Assign_card

exception Empty
exception InvaidDouble of int * int
exception InvaidTriple of int * int * int
exception InvaidForm

type choice =
  | Continue of int list
  | Skip

(** [compare_card x y] compares two cards x and y, returns 1 if card x has a
    higher rank than the card y, -1 if x has a smaller rank than y, and 0 if
    they have the same rank. Note that jokers are the largest *)
let compare_card x y =
  if x = 54 then 1
  else if y = 54 then -1
  else if x = 53 then 1
  else if y = 53 then -1
  else
    let x_num = x mod 13 in
    let y_num = y mod 13 in
    if x_num < y_num then -1 else if x_num > y_num then 1 else 0

(** [sort cards] sorts the list of cards in ascending order based on card rank *)
let sorted (cards : int list) : int list = List.sort compare_card cards

let sorted_uniq (cards : int list) : int list =
  List.sort_uniq compare_card cards

(** [single this other] returns a single card to put down in response to the
    single card the other player just put down. Returns [Continue card] where
    card is the list of card to put down if there is a card in [this] with a
    greater rank than the opponent's card and [Skip] if there are no cards
    greater. Requires: [this] is not empty, and [other] contains only one
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

(** [single this other] returns the card to put down in response to what the
    other player just put down, returns Continue [card] if there is a card in
    [this] with a greater rank than the opponent’s card, returns Skip if there
    are no cards greater *)
let rec single (this : int list) (other : int list) : choice =
  match this with
  | [] -> Skip
  | h :: t ->
      if compare_card h (List.hd other) = 1 then Continue [ h ]
      else single t other

(** [straight this other] returns a straight to put down in response to the
    straight the other player just put down. Returns [Continue card] where card
    is the list of cards to put down if there is a straight in [this] with
    greater ranks than the opponent's straight and [Skip] if there are no cards
    greater. Requires: [this] is not empty, and [other] is a list of five
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

(** [double lst] returns [Continue card] where card is a list of four cards of
    the same rank if there is a two-of-a-kind in [lst] and [Skip] otherwise *)
let rec double (lst : int list) : choice =
  match sorted (remove_joker lst) with
  | [] | [ _ ] -> Skip
  | c1 :: c2 :: t ->
      if c1 mod 13 = c2 mod 13 then Continue [ c1; c2 ] else double (c2 :: t)

(** [triplet lst] returns [Continue card] where card is a list of four cards of
    the same rank if there is a three-of-a-kind in [lst] and [Skip] otherwise *)
let rec triplet (lst : int list) : choice =
  match sorted (remove_joker lst) with
  | [] | [ _ ] | [ _; _ ] -> Skip
  | c1 :: c2 :: c3 :: t ->
      if c1 mod 13 = c2 mod 13 && c2 mod 13 = c3 mod 13 then
        Continue [ c1; c2; c3 ]
      else triplet (c2 :: c3 :: t)

(** [four lst] returns [Continue card] where card is a list of four cards of the
    same rank if there is a four-of-a-kind in [lst] and [Skip] otherwise *)
let rec four (lst : int list) : choice =
  match sorted (remove_joker lst) with
  | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] -> Skip
  | c1 :: c2 :: c3 :: c4 :: t ->
      if c1 mod 13 = c2 mod 13 && c2 mod 13 = c3 mod 13 && c3 mod 13 = c4 mod 13
      then Continue [ c1; c2; c3; c4 ]
      else four (c2 :: c3 :: c4 :: t)

(** [joker_pair lst] returns [Continue card] where card is a list of two jokers
    if there are two jokers in [lst] and [Skip] otherwise *)
let rec joker_pair (lst : int list) : choice =
  match sorted lst with
  | [] | [ _ ] -> Skip
  | _ :: [ 53; 54 ] -> Continue [ 53; 54 ]
  | _ -> Skip
