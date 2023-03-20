open Assign_card

type choice =
  | Continue of int list
  | Skip

exception Empty
exception InvaidDouble of int * int
exception InvaidTriple of int * int * int
exception InvaidForm

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

(** [sort cards] sorts the list of cards in ascending order based on card rank *)
let sort (cards : int list) : int list = List.sort compare_card cards

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
