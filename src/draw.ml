let number_to_card (card_number : int) =
  if card_number = 53 || card_number = 54 then 0
  else
    let x = (card_number + 2) mod 13 in
    if x <> 0 then x else 13

let number_to_suit (card_number : int) =
  if card_number > 0 && card_number <= 13 then "♢"
  else if card_number > 13 && card_number <= 26 then "♣"
  else if card_number > 26 && card_number <= 39 then "♡"
  else if card_number > 39 && card_number <= 52 then "♠"
  else if card_number = 53 then "🃏"
  else "🃏"

let numbers_to_card (card_numbers : int list) =
  List.map number_to_card card_numbers

let print_card (card : int) =
  if number_to_card card = 11 then Printf.sprintf "|%sJ|" (number_to_suit card)
  else if number_to_card card = 12 then
    Printf.sprintf "|%sQ|" (number_to_suit card)
  else if number_to_card card = 13 then
    Printf.sprintf "|%sK|" (number_to_suit card)
  else Printf.sprintf "|%s%i%!|" (number_to_suit card) (number_to_card card)

let print_cards (cards : int list) = List.map print_card cards

let print =
  print_endline " ";
  print_endline "Camels are bae"
