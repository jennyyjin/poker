open Comparison

(** [number_to_card card_number] takes the serial number of the card and return
    its number shown on the card. *)
let number_to_card (card_number : int) =
  if card_number = 52 || card_number = 53 then 0
  else
    let x = (card_number + 3) mod 13 in
    if x <> 0 then x else 13

(** [number_to_suit card_number] takes the serial number of the card and return
    its suit shown on the card. *)
let number_to_suit (card_number : int) =
  if card_number >= 0 && card_number < 13 then "♢"
  else if card_number >= 13 && card_number < 26 then "♣"
  else if card_number >= 26 && card_number < 39 then "♡"
  else if card_number >= 39 && card_number < 52 then "♠"
  else if card_number = 52 then "🃟"
  else "🃏"

(** [print_card card_number] takes the serial number of the card and return the
    string for its drawing on board *)
let print_card (card_number : int) =
  if number_to_card card_number = 0 then
    Printf.sprintf "|%s|" (number_to_suit card_number)
  else if number_to_card card_number = 11 then
    Printf.sprintf "|%sJ|" (number_to_suit card_number)
  else if number_to_card card_number = 12 then
    Printf.sprintf "|%sQ|" (number_to_suit card_number)
  else if number_to_card card_number = 13 then
    Printf.sprintf "|%sK|" (number_to_suit card_number)
  else if number_to_card card_number = 1 then
    Printf.sprintf "|%sA|" (number_to_suit card_number)
  else
    Printf.sprintf "|%s%i%!|"
      (number_to_suit card_number)
      (number_to_card card_number)

(** [print_card card_number] takes a list of serial numbers of the cards and
    return the string for their drawing on board *)
let print_cards (cards : int list) =
  String.concat " " (List.map print_card cards)

(** Guide on top of the board for player's turn*)
let guide_player = "\n\nIt's your turn!\n"

(** Guide on top of the board for AIs's turn*)
let guide_ai = "\n\nUpdated Game Board!\n"

(** Board Top*)
let top_ai = "--------------------------------------------------------------"

(** Board Bottom*)
let bottom_ai = "--------------------------------------------------------------"

(** Board Top*)
let top_board = "=============================================================="

(** Board Bottom*)
let bottom_board =
  "=============================================================="

(** Indices Indicator*)
let indices =
  "          Indices : | 0| | 1| | 2| | 3| | 4| | 5| | 6| | 7| | 8| | 9| |10| \
   |11| |12| | 13| |14| |15| |16| |17| |18| |19|"

(** [print_board ] takes current cards state and print the board when it's
    player's turn*)
let print_board (fst_ai_cards : int list) (snd_ai_cards : int list)
    (player_cards : int list) (prev_cards : int list) =
  guide_player ^ top_board ^ "\n"
  ^ Printf.sprintf "Player1's number of cards left: %i"
      (List.length fst_ai_cards)
  ^ "\n"
  ^ Printf.sprintf "Player2's number of cards left: %i"
      (List.length snd_ai_cards)
  ^ "\n" ^ "Cards put down by AI: " ^ print_cards prev_cards ^ "\n"
  ^ "Your cards in hand: " ^ print_cards player_cards ^ "\n" ^ indices ^ "\n"
  ^ bottom_board

(** [print_ai_board ] takes current cards state and print the board when it's
    first AI's turn*)
let print_ai_board (fst_ai_cards : int list) (snd_ai_cards : int list)
    (player_cards : int list) (prev_cards : int list) =
  guide_ai ^ top_ai ^ "\n"
  ^ Printf.sprintf "Player1's number of cards left: %i"
      (List.length fst_ai_cards)
  ^ "\n"
  ^ Printf.sprintf "Player2's number of cards left: %i"
      (List.length snd_ai_cards)
  ^ "\n" ^ "Cards put down by you: " ^ print_cards prev_cards ^ "\n"
  ^ "Your cards in hand: " ^ print_cards player_cards ^ "\n" ^ indices ^ "\n"
  ^ bottom_ai

(** [print_snd_ai_board ] takes current cards state and print the board when
    it's second AI's turn*)
let print_snd_ai_board (fst_ai_cards : int list) (snd_ai_cards : int list)
    (player_cards : int list) (prev_cards : int list) =
  guide_ai ^ top_ai ^ "\n"
  ^ Printf.sprintf "Card on Board: %i" (List.length fst_ai_cards)
  ^ "\n"
  ^ Printf.sprintf "Player2's number of cards left: %i"
      (List.length snd_ai_cards)
  ^ "\n" ^ "Card on Board: " ^ print_cards prev_cards ^ "\n"
  ^ "Your cards in hand: " ^ print_cards player_cards ^ "\n" ^ indices ^ "\n"
  ^ bottom_ai
