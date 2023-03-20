exception Empty

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

(** [sort cards] sorts the list of cards in ascending order based on card rank *)
let sort (cards : int list) : int list = List.sort compare_card cards

(** [single this other] returns the card to put down in response to what the
    other player just put down, returns Continue [card] if there is a card in
    [this] with a greater rank than the opponent's card, returns Skip if there
    are no cards greater *)
let rec single (this : int list) (other : int list) : choice =
  if this = [] then raise Empty
  else
    match this with
    | [] -> Skip
    | h :: t ->
        if compare_card h (List.hd other) = 1 then Continue [ h ]
        else single t other
