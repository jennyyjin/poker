open Play

type card =
  | Spade of int
  | Heart of int
  | Diamond of int
  | Club of int

let card_list = List.init 54 (fun i -> i)

let random n =
  Random.self_init ();
  Random.int n

let rec shuffling rest_cards current_deck =
  let num = List.length rest_cards in
  match num with
  | 0 -> current_deck
  | _ ->
      let chosen_index = random num in
      let chosen_card = List.nth rest_cards chosen_index in
      let rest_cards = update_ai_cards rest_cards [ chosen_card ] in
      shuffling rest_cards current_deck @ [ chosen_card ]

(** [scrambled_list] is the 54 cards in a poker deck and we name them from 1 to
    54 and scramble them up*)
let scrambled_list = List.sort (fun _ _ -> random 3 - 1) card_list

(** [split_in_half lst] splits up the cards into two decks.*)
let rec split_in_half lst =
  match lst with
  | [] -> ([], [])
  | [ x ] -> ([ x ], [])
  | x :: y :: rest ->
      let first_half, second_half = split_in_half rest in
      (x :: first_half, y :: second_half)

let rec split_in_three lst =
  match lst with
  | [] -> ([], [], [])
  | [ x ] -> ([ x ], [], [])
  | [ x; y ] -> ([ x ], [ y ], [])
  | [ x; y; z ] -> ([], [], [ x; y; z ])
  | x :: y :: z :: rest ->
      let first, second, third = split_in_three rest in
      (x :: first, y :: second, z :: third)
