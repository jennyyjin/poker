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
