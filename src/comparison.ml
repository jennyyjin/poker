open Assign_card

exception Empty
exception InvaidDouble of int * int
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

(** [sorted cards] sorts the list of cards in ascending order based on card rank *)
let sorted (cards : int list) : int list = List.sort compare_card cards

(** [sorted cards] sorts the list of cards in ascending order based on card rank
    and removes duplicates *)
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

(** [single_greater_lst this other] returns all of the cards that are greater
    than the card that other player just put down. Requires that the card put
    down by the other player is only one card*)
let rec single_greater_lst (this : int list) (other : int) =
  match this with
  | h :: t ->
      if compare h other = 1 then h :: single_greater_lst t other
      else single_greater_lst t other
  | [] -> []

(** [remove_non_duplicates] removes all the non duplicates from the lists*)
let rec remove_non_duplicates = function
  | [] -> []
  | x :: xs ->
      if List.mem x xs then x :: x :: remove_non_duplicates xs
      else remove_non_duplicates xs

(** [first_two_element lst] returns first two elements of the list*)
let first_two_element lst =
  match lst with
  | x :: y :: _ -> [ x; y ]
  | _ -> []

(** [double_helper this other] returns Continue [card] if there is a card in
    [this] with a greater rank than the opponent’s card, returns Skip if there
    are no cards greater. Requries that other is a valid double card] *)
(* let double_helper (this : int list) (other : int list) = let dup_list =
   single_greater_lst this (List.hd other) |> remove_non_duplicates in if
   dup_list <> [] then let double_lst = dup_list |> sort |> first_two_element in
   Continue double_lst else Skip *)

(** [double this other] assures that the other's card is a double, returns
    Continue [card] if there is a card in this with a greater rank than the
    opponent’s [card], returns Skip if there are no cards greater. If other's
    card is a not valid two card combo, raise \[InvaidDouble(card1, card2)]\. If
    other's deck is not a two card combo, raise \[InvaidForm]\ *)
(* let double (this : int list) (other : int list) = match other with | [ h; t ]
   -> if h <> t then raise (InvaidDouble (h, t)) else double_helper this other |
   _ -> raise InvaidForm *)
