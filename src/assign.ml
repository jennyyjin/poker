open Play

let card_list () = List.init 54 (fun i -> i)

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

let scrambled_list () = shuffling (card_list ()) []

let rec split_in_three lst =
  match lst with
  | [] -> ([], [], [])
  | [ x ] -> ([ x ], [], [])
  | [ x; y ] -> ([ x ], [ y ], [])
  | [ x; y; z ] -> ([], [], [ x; y; z ])
  | x :: y :: z :: rest ->
      let first, second, third = split_in_three rest in
      (x :: first, y :: second, z :: third)
