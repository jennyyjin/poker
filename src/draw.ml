(** Module Draw draws the game borard and the representation of the cards to the
    users*)

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

(** [print_card card_number] converts a card into a string format for better
    display *)
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

(** [print_cards cards] returns a string of cards to beat *)
let print_cards (cards : int list) =
  String.concat " " (List.map print_card cards)

(** [display_prev_cards cards] returns a string to display cards to beat *)
let display_prev_cards (cards : int list) =
  if print_cards cards = "" then "None! You can start a new pattern"
  else print_cards cards

(** Guide on top of the board for player's turn*)
let guide_player = "\n\nIt's your turn!\n"

(** Guide on top of the board for player's first turn*)
let guide_fst_player = "\n\nYou start the game!\n"

(** Guide on top of the board for player's invalid turn*)
let guide_invalid_player = "\n\nBe careful with input rule!\n"

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

(** Board Top*)
let top_board (player_cards : int list) =
  let new_player_cards = List.map (fun x -> string_of_int x) player_cards in
  let board_lst = List.map (fun x -> "-----") new_player_cards in
  List.fold_left (fun acc x -> acc ^ x) "" board_lst

(** Board Bottom*)
let bottom_board (player_cards : int list) =
  let new_player_cards = List.map (fun x -> string_of_int x) player_cards in
  let board_lst = List.map (fun x -> "=====") new_player_cards in
  List.fold_left (fun acc x -> acc ^ x) "" board_lst

(** [print_board ] takes current cards state and print the board when it's
    player's turn*)
let print_board (fst_ai_cards : int list) (snd_ai_cards : int list)
    (player_cards : int list) (prev_cards : int list) =
  guide_player ^ top_board player_cards ^ "\n"
  ^ Printf.sprintf "Justin's number of cards left: %i"
      (List.length fst_ai_cards)
  ^ "\n"
  ^ Printf.sprintf "Kozen's number of cards left: %i" (List.length snd_ai_cards)
  ^ "\n" ^ "Card(s) to beat: "
  ^ display_prev_cards prev_cards
  ^ "\n" ^ "Your cards in hand: \n" ^ print_cards player_cards ^ "\n"
  ^ indices player_cards ^ "\n" ^ bottom_board player_cards ^ "\n"
(* ^ print_cards fst_ai_cards ^ "\n" ^ print_cards snd_ai_cards *)

(** [print_fst_board ] takes the beginning cards state and print the board*)
let print_fst_board (fst_ai_cards : int list) (snd_ai_cards : int list)
    (player_cards : int list) =
  guide_fst_player ^ top_board player_cards ^ "\n"
  ^ Printf.sprintf "Justin's number of cards left: %i"
      (List.length fst_ai_cards)
  ^ "\n"
  ^ Printf.sprintf "Kozen's number of cards left: %i" (List.length snd_ai_cards)
  ^ "\n" ^ "Your cards in hand: \n" ^ print_cards player_cards ^ "\n"
  ^ indices player_cards ^ "\n" ^ bottom_board player_cards ^ "\n"
(* ^ print_cards fst_ai_cards ^ "\n" ^ print_cards snd_ai_cards *)

(** [print_fst_board ] prints the board when the user has invalid input *)
let print_invalid_board (fst_ai_cards : int list) (snd_ai_cards : int list)
    (player_cards : int list) (prev_cards : int list) =
  top_board player_cards ^ "\n"
  ^ Printf.sprintf "Justin's number of cards left: %i"
      (List.length fst_ai_cards)
  ^ "\n"
  ^ Printf.sprintf "Kozen's number of cards left: %i" (List.length snd_ai_cards)
  ^ "\n" ^ "Cards to beat: " ^ print_cards prev_cards ^ "\n"
  ^ "Your cards in hand: \n" ^ print_cards player_cards ^ "\n"
  ^ indices player_cards ^ "\n" ^ bottom_board player_cards ^ "\n"
(* ^ print_cards fst_ai_cards ^ "\n" ^ print_cards snd_ai_cards *)

(** [print_player_choice player_choice] outputs player's choice of cards to put
    down *)
let print_player_choice (player_choice : int list) =
  match player_choice with
  | [] -> "\n" ^ "You: Skip" ^ "\n"
  | _ -> "\n" ^ "You: " ^ print_cards player_choice ^ "\n"

(** [print_ai_choice ai_choice] outputs the first ai's choice of cards to put
    down *)
let print_ai_choice (ai_choice : int list) =
  match ai_choice with
  | [] -> "\n" ^ "AI Justin: Skip" ^ "\n"
  | _ -> "\n" ^ "AI Justin:" ^ print_cards ai_choice ^ "\n"

(** [print_ai2_choice ai2_choice] outputs the second ai's choice of cards to put
    down *)
let print_ai2_choice (ai2_choice : int list) =
  match ai2_choice with
  | [] -> "\n" ^ "AI Kozen: Skip" ^ "\n"
  | _ -> "\n" ^ "AI Kozen:" ^ print_cards ai2_choice ^ "\n"
