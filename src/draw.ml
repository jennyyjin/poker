open Comparison
open Helper

let number_to_card (card_number : int) = Helper.number_to_card card_number
let number_to_suit (card_number : int) = Helper.number_to_suit card_number

let print_card (card_number : int) =
  if number_to_card card_number = 0 then
    Printf.sprintf "|%s|" (number_to_suit card_number)
  else if number_to_card card_number = 10 then
    Printf.sprintf "|%sX|" (number_to_suit card_number)
  else if number_to_card card_number = 10 then
    Printf.sprintf "|%sX|" (number_to_suit card_number)
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

let print_cards (cards : int list) =
  String.concat " " (List.map print_card cards)

(** Guide on top of the board for player's turn*)
let guide_player = "\n\nIt's your turn!\n"

(** Guide on top of the board for AIs's turn*)
let guide_ai1 = "\n\nIt's Player 1 turn!\n"

let guide_ai2 = "\n\nIt's Player 2 turn!\n"

(** Board Top*)
let top_ai = "--------------------------------------------------------------"

(** Board Bottom*)
let bottom_ai = "--------------------------------------------------------------"

(** Board Top*)
let top_board = "=============================================================="

(** Board Bottom*)
let bottom_board =
  "=============================================================="

(** [indices_helper i ] convert i to string '| i|' or '|i|'*)
let indices_helper i =
  if i < 10 then "| " ^ string_of_int i ^ "| " else "|" ^ string_of_int i ^ "| "

(** [indices_lst i ] returns lst ['|0|', '|1|', ..., '|i|']*)
let rec indices_list i =
  if i < 0 then [] else indices_list (i - 1) @ [ indices_helper i ]

(** [indices] returns the indice bar used for the game*)
let indices (player_cards : int list) =
  let cards_size = List.length player_cards - 1 in
  let indices_l = indices_list cards_size in
  List.fold_left (fun acc x -> acc ^ x) "" indices_l

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
  ^ "\n" ^ "Cards to beat: " ^ print_cards prev_cards ^ "\n"
  ^ "Your cards in hand: \n" ^ print_cards player_cards ^ "\n"
  ^ indices player_cards ^ "\n" ^ bottom_board ^ "\n" ^ print_cards fst_ai_cards
  ^ "\n" ^ print_cards snd_ai_cards

(** [print_ai_board ] takes current cards state and print the board when it's
    first AI's turn*)
let print_ai_board (fst_ai_cards : int list) (snd_ai_cards : int list)
    (player_cards : int list) (prev_cards : int list) =
  guide_ai1 ^ top_ai ^ "\n"
  ^ Printf.sprintf "Player1's number of cards left: %i"
      (List.length fst_ai_cards)
  ^ "\n"
  ^ Printf.sprintf "Player2's number of cards left: %i"
      (List.length snd_ai_cards)
  ^ "\n" ^ "Cards to beat: " ^ print_cards prev_cards ^ "\n"
  ^ print_cards fst_ai_cards ^ "\n" ^ print_cards snd_ai_cards

(** [print_snd_ai_board ] takes current cards state and print the board when
    it's second AI's turn*)
let print_snd_ai_board (fst_ai_cards : int list) (snd_ai_cards : int list)
    (player_cards : int list) (prev_cards : int list) =
  guide_ai2 ^ top_ai ^ "\n"
  ^ Printf.sprintf "Player1's number of cards left: %i"
      (List.length fst_ai_cards)
  ^ "\n"
  ^ Printf.sprintf "Player2's number of cards left: %i"
      (List.length snd_ai_cards)
  ^ "\n" ^ "Cards to beat: " ^ print_cards prev_cards ^ "\n"
  ^ print_cards fst_ai_cards ^ "\n" ^ print_cards snd_ai_cards

let print_player_choice (player_choice : int list) =
  match player_choice with
  | [] -> "Player: Skip" ^ "\n"
  | _ -> "\n" ^ "Player:" ^ print_cards player_choice ^ "\n"

let print_ai_choice (ai_choice : int list) =
  match ai_choice with
  | [] -> "AI Apple: Skip" ^ "\n"
  | _ -> "\n" ^ "AI Apple:" ^ print_cards ai_choice ^ "\n"

let print_ai2_choice (ai2_choice : int list) =
  match ai2_choice with
  | [] -> "AI Banana: Skip" ^ "\n"
  | _ -> "\n" ^ "AI Banana:" ^ print_cards ai2_choice ^ "\n"
