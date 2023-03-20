open Assign_card

type choice =
  | Continue of int list
  | Skip

exception Empty
exception InvaidDouble of int * int
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
let double_helper (this : int list) (other : int list) =
  let dup_list =
    single_greater_lst this (List.hd other) |> remove_non_duplicates
  in
  if dup_list <> [] then
    let double_lst = dup_list |> sort |> first_two_element in
    Continue double_lst
  else Skip

(** [double this other] assures that the other's card is a double, returns
    Continue [card] if there is a card in this with a greater rank than the
    opponent’s [card], returns Skip if there are no cards greater. If other's
    card is a not valid two card combo, raise \[InvaidDouble(card1, card2)]\. If
    other's deck is not a two card combo, raise \[InvaidForm]\ *)
let double (this : int list) (other : int list) =
  match other with
  | [ h; t ] ->
      if h <> t then raise (InvaidDouble (h, t)) else double_helper this other
  | _ -> raise InvaidForm
