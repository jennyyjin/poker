open Assign

exception Empty
exception Wrong

type choice =
  | Continue of int list
  | Skip
  | Other

(** [compare_card x y] compares two cards x and y, returns 1 if card x has a
    higher rank than the card y, -1 if x has a smaller rank than y, and 0 if
    they have the same rank. Note that jokers are the largest *)
let compare_card x y =
  if x = 53 then 1
  else if y = 53 then -1
  else if x = 52 then 1
  else if y = 52 then -1
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
    greater rank than the opponent's card and [Other] if there are no cards
    greater. Requires: [this] is not empty, and [other] contains only one
    integer *)
let rec single (this : int list) (other : int list) : choice =
  match this with
  | [] -> Other
  | h :: t ->
      if compare_card h (List.hd other) = 1 then Continue [ h ]
      else single t other

(** [remove_joker lst] returns a list of cards with jokers removed *)
let rec remove_joker (lst : int list) =
  match lst with
  | [] -> []
  | h :: t -> if h = 52 || h = 53 then remove_joker t else h :: remove_joker t

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
    greater ranks than [other] and [Other] if there are no cards greater.
    Requires: [this] is not empty, and [other] is a list of five consecutive
    numbers with no duplicates *)
let rec straight (this : int list) (other : int list) : choice =
  match sorted_uniq (remove_joker this) with
  | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] | [ _; _; _; _ ] -> Other
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

(** [double_helper lst] only contains duplicates *)
let rec double_helper (lst : int list) : int list =
  match sorted (remove_joker lst) with
  | [] | [ _ ] -> []
  | c1 :: c2 :: t ->
      if c1 mod 13 = c2 mod 13 then c1 :: c2 :: double_helper t
      else double_helper (c2 :: t)

(** [double_lst this other] returns a list of a pair of cards to put down if
    there is a pair in [this] greater than [other] and returns an empty list
    otherwise *)
let rec double_lst (this : int list) (other : int list) : int list =
  match double_helper this with
  | [] | [ _ ] -> []
  | c1 :: c2 :: t ->
      if compare_card c1 (List.hd other) = 1 then [ c1; c2 ]
      else double_lst t other

(** [double this other] returns [Continue card] where card is a pair of the same
    rank if there is a pair in [this] greater than [other] and [Other] otherwise *)
let double (this : int list) (other : int list) : choice =
  match double_lst this other with
  | [] -> Other
  | [ c1; c2 ] -> Continue [ c1; c2 ]
  | _ -> raise Wrong

(** [triple_helper lst] only contains triples *)
let rec triple_helper (lst : int list) : int list =
  match sorted (remove_joker lst) with
  | [] | [ _ ] | [ _; _ ] -> []
  | c1 :: c2 :: c3 :: t ->
      if c1 mod 13 = c2 mod 13 && c2 mod 13 = c3 mod 13 then
        [ c1; c2; c3 ] @ triple_helper t
      else triple_helper (c2 :: c3 :: t)

(** [triple_lst this other] returns a list of a triple to put down if there is a
    triple in [this] greater than [other] and returns an empty list otherwise *)
let rec triple_lst (this : int list) (other : int list) : int list =
  match triple_helper this with
  | [] | [ _ ] | [ _; _ ] -> []
  | c1 :: c2 :: c3 :: t ->
      if compare_card c1 (List.hd other) = 1 then [ c1; c2; c3 ]
      else triple_lst t other

(** [triple this other] returns [Continue card] where card is a triple of the
    same rank if there is a triple in [this] greater than [other] and [Other]
    otherwise *)
let triple (this : int list) (other : int list) : choice =
  match triple_lst this other with
  | [] -> Other
  | [ c1; c2; c3 ] -> Continue [ c1; c2; c3 ]
  | _ -> raise Wrong

(** [quad lst] returns [Continue card] where card is a list of four cards of the
    same rank if there is a four-of-a-kind in [lst] and [Other] otherwise *)
let rec quad (lst : int list) : choice =
  match sorted (remove_joker lst) with
  | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] -> Other
  | c1 :: c2 :: c3 :: c4 :: t ->
      if c1 mod 13 = c2 mod 13 && c2 mod 13 = c3 mod 13 && c3 mod 13 = c4 mod 13
      then Continue [ c1; c2; c3; c4 ]
      else quad (c2 :: c3 :: c4 :: t)

(** [joker_pair lst] returns [Continue card] where card is a list of two jokers
    if there are two jokers in [lst] and [Skip] otherwise *)
let rec joker_pair (lst : int list) : choice =
  match sorted lst with
  | [] | [ _ ] -> Skip
  | _ :: [ 52; 53 ] -> Continue [ 52; 53 ]
  | _ -> Skip

(** [triple_p_double this other] returns [Continue card] where card is a list of
    four cards of the same rank if there is a two-of-a-kind in [lst] and [Other]
    otherwise *)
let rec triple_p_double (this : int list) (other : int list) : choice =
  match this with
  | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] | [ _; _; _; _ ] -> Other
  | _ -> (
      match triple_helper this with
      | [] | [ _ ] | [ _; _ ] -> Other
      | c1 :: c2 :: c3 :: t ->
          if List.hd (double_lst this other) <> List.hd (triple_lst this other)
          then
            if compare_card c1 (List.hd other) = 1 then Continue [ c1; c2; c3 ]
            else triple t other
          else Other)
