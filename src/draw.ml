open Comparison

let number_to_card (card_number : int) =
  if card_number = 52 || card_number = 53 then 0
  else
    let x = (card_number + 3) mod 13 in
    if x <> 0 then x else 13

let number_to_suit (card_number : int) =
  if card_number >= 0 && card_number < 13 then "♢"
  else if card_number >= 13 && card_number < 26 then "♣"
  else if card_number >= 26 && card_number < 39 then "♡"
  else if card_number >= 39 && card_number < 52 then "♠"
  else if card_number = 52 then "🃟"
  else "🃏"

let numbers_to_card (card_numbers : int list) =
  List.map number_to_card card_numbers

let print_card (card : int) =
  if number_to_card card = 0 then Printf.sprintf "|%s|" (number_to_suit card)
  else if number_to_card card = 11 then
    Printf.sprintf "|%sJ|" (number_to_suit card)
  else if number_to_card card = 12 then
    Printf.sprintf "|%sQ|" (number_to_suit card)
  else if number_to_card card = 13 then
    Printf.sprintf "|%sK|" (number_to_suit card)
  else Printf.sprintf "|%s%i%!|" (number_to_suit card) (number_to_card card)

let print_cards (cards : int list) =
  String.concat " " (List.map print_card cards)

let top_board = "=============================================================="

let bottom_board =
  "=============================================================="

let print_board (ai_cards : int list) (player_cards : int list)
    (ai_prev : int list) (play_prev : int list) =
  top_board ^ "\n"
  ^ Printf.sprintf "Player1 number of left cards: %i" (List.length ai_cards)
  ^ "\n" ^ "Player1 last output: " ^ print_cards ai_prev ^ "\n"
  ^ "Your last output: " ^ print_cards play_prev ^ "\n" ^ "Your cards left: "
  ^ print_cards player_cards ^ "\n" ^ bottom_board
