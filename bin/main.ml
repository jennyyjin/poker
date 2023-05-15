open Poker
open Draw
open Comparison
open Assign
open Play
open Ai

exception Format

let prev_cards = []
let turn = 0

(** [check_empty str] is a boolean of whether or not [str] is an empty string *)
let check_empty (str : string) = String.length str <> 0

(** [play_game ai_cards player_cards prev_cards turn] runs the poker game,
    allowing player to put down cards *)
let rec play_game fst_ai_cards snd_ai_cards player_cards prev_cards turn
    same_cards_count choice beginning prev_turn =
  try
    let prev_cards = if same_cards_count mod 3 = 2 then [] else prev_cards in
    if List.length fst_ai_cards = 0 || List.length snd_ai_cards = 0 then (
      if turn = 0 then print_string (print_ai2_choice choice ^ "⬆️ Winner!")
      else print_string (print_ai_choice choice ^ "⬆️ Winner!");
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n\nSorry, you lost! \n(\\__/)\n(⁰̷̴͈꒨⁰̷̴͈)\n/ > \n")
    else if List.length player_cards = 0 then
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "\n\nCongratulations! You won! \n (\\__/)\n( •̀ᴗ•́ )\n/ > \n";
    if
      List.length fst_ai_cards = 0
      || List.length snd_ai_cards = 0
      || List.length player_cards = 0
    then exit 0;
    if beginning = 0 then
      print_string
        ("\n" ^ Draw.print_fst_board fst_ai_cards snd_ai_cards player_cards)
    else if beginning = -1 then
      print_string
        ("\n"
        ^ Draw.print_invalid_board fst_ai_cards snd_ai_cards player_cards
            prev_cards)
    else if turn = 0 then
      print_string
        (Draw.print_ai2_choice choice
        ^ "\n"
        ^ Draw.print_board fst_ai_cards snd_ai_cards player_cards prev_cards)
    else if turn = 1 then print_string (Draw.print_player_choice choice)
      (* print_string (Draw.print_ai_board fst_ai_cards snd_ai_cards
         player_cards prev_cards) *)
    else print_string (Draw.print_ai_choice choice);
    (* print_string (Draw.print_snd_ai_board fst_ai_cards snd_ai_cards
       player_cards prev_cards); *)
    if turn = 0 then
      print_string
        "\n\
         Please input the indices of the cards you want to put down (separated \
         by space) or enter an empty space to skip: \n";
    if turn = 0 then
      let user_input = read_line () in
      if user_input = "quit" then exit 0
      else
        let input =
          List.map
            (fun e -> int_of_string e)
            (List.filter check_empty (String.split_on_char ' ' user_input))
        in
        let player_placed_cards = index_to_num player_cards input in
        let checkvalid =
          Comparison.check_valid player_placed_cards prev_cards
        in
        if checkvalid = false then raise Format
        else
          match player_placed_cards with
          | [] ->
              play_game fst_ai_cards snd_ai_cards player_cards prev_cards 1
                (same_cards_count + 1) player_placed_cards 1 (-1)
          | _ ->
              let prev_cards = index_to_num player_cards input in
              let player_cards = update_cards player_cards input in
              play_game fst_ai_cards snd_ai_cards player_cards prev_cards 1 0
                player_placed_cards 1 0
    else if turn = 1 then
      let input =
        if
          (prev_turn = 2 && Ai.collab prev_cards = true) || same_cards_count = 1
        then []
        else
          match Ai.play fst_ai_cards prev_cards player_cards with
          | Skip | Other -> []
          | Continue cards -> cards
      in
      match input with
      | [] ->
          play_game fst_ai_cards snd_ai_cards player_cards prev_cards 2
            (same_cards_count + 1) input 1 (-1)
      | _ ->
          let prev_cards = input in
          let fst_ai_cards = update_ai_cards fst_ai_cards input in
          play_game fst_ai_cards snd_ai_cards player_cards prev_cards 2 0 input
            1 1
    else if turn = 2 then
      let input =
        if prev_turn = 1 && Ai.collab prev_cards = true then []
        else
          match Ai.play snd_ai_cards prev_cards player_cards with
          | Skip | Other -> []
          | Continue cards -> cards
      in
      match input with
      | [] ->
          play_game fst_ai_cards snd_ai_cards player_cards prev_cards 0
            (same_cards_count + 1) input 1 (-1)
      | _ ->
          let prev_cards = input in
          let snd_ai_cards = update_ai_cards snd_ai_cards input in
          play_game fst_ai_cards snd_ai_cards player_cards prev_cards 0 0 input
            1 2
  with _ ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\nYour input is invalid. Please try again.";

    play_game fst_ai_cards snd_ai_cards player_cards prev_cards 0
      same_cards_count choice (-1) (-1)

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to Poker.\n\n";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Rules for the game: \n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "The game starts with you, and the turns will go in order \
     You-AI1-AI2-You-AI1-AI2...\n\
     Each turn, each player is only allowed to place 0 cards (skip), 1 card, 2 \
     cards, 3 cards, 4 cards or 5 cards. \n\
     Card rank in increasing order: 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K, A, 2, 🃟, 🃏\n\
     The suit of cards doesn't matter. \n\
     1 single card can only beat other single cards of a smaller rank. \n\
     2 cards must be a pair of the same rank, and it can only beat other pairs \
     of a smaller rank. \n\
     3 cards must be a triple of the same rank, and it can only beat other \
     triples of a smaller rank. \n\
     Bombs can beat any non-bomb cards. \n\
     A pair of 🃟 🃏 is the most powerful bomb, and 4 cards of the same rank is \
     another type of bomb. \n\
     The player that gets rid of all of their cards first wins! Try to beat \
     both AIs! You got this!";
  let deck = Assign.scrambled_list () in
  let cards_group = split_in_three deck in
  let a, b, c = cards_group in
  let fst_ai_cards = sorted a in
  let snd_ai_cards = sorted b in
  let player_cards = sorted c in
  let prev_cards = [] in
  let prev_turn = -1 in
  let turn = 0 in
  let same_cards_count = 0 in
  let choice = [] in
  let beginning = 0 in
  play_game fst_ai_cards snd_ai_cards player_cards prev_cards turn
    same_cards_count choice beginning prev_turn

let () = main ()
