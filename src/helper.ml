(** [number_to_card x] convert x to the number on the card that is represented
    by x *)
let number_to_card (card_number : int) =
  if card_number = 52 || card_number = 53 then 0
  else
    let x = (card_number + 3) mod 13 in
    if x <> 0 then x else 13

(** [number_to_suit x] convert x to the suit on the card that is represented by
    x *)
let number_to_suit (card_number : int) =
  if card_number >= 0 && card_number < 13 then "♢"
  else if card_number >= 13 && card_number < 26 then "♣"
  else if card_number >= 26 && card_number < 39 then "♡"
  else if card_number >= 39 && card_number < 52 then "♠"
  else if card_number = 52 then "🃟 "
  else "🃏"
