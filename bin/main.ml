open Poker
open Draw
open Comparison
open Assign
open Play

exception Format

(** [card_list] is the 54 cards in a poker deck and we name them from 1 to 54 *)
(* let make_card_list = List.init 54 (fun i -> i) *)
(** [scrambled_list] is the 54 cards in a poker deck and we name them from 1 to
    54 and scramble them up*)
(* let make_scrambled_list = List.sort (fun _ _ -> Random.int 3 - 1) make_card_list *)
(* 
let cards_group = split_in_half make_scrambled_list *)
(* let ai_cards = sorted (fst cards_group)
let player_cards = sorted (snd cards_group) *)
let prev_cards = []
let turn = 0

(** [check_empty str] is a boolean of whether or not [str] is an empty string *)
let check_empty (str : string) = String.length str <> 0

(** [play_game ai_cards player_cards prev_cards turn] runs the poker game,
    allowing player to put down cards *)
let rec play_game ai_cards player_cards prev_cards turn =
  try
  if List.length ai_cards = 0 then
    ANSITerminal.print_string [ ANSITerminal.green ] "\n\n Sorry, you lost! \n"
  else if List.length player_cards = 0 then
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\n\n Congratulations! You won! \n";
  if List.length ai_cards = 0 || List.length player_cards = 0 then exit 0;
  if turn = 0 then
    print_string (Draw.print_board ai_cards player_cards prev_cards)
  else print_string (Draw.print_ai_board ai_cards player_cards prev_cards);
  if turn = 0 then
    print_string
      "\nPlease input the indices of the cards you want to put down: \n";
  if turn = 0 then
    let input = 
      List.map
        (fun e -> int_of_string e)
        (List.filter check_empty (String.split_on_char ' ' (read_line ())))
    in
    let prev_cards = index_to_num player_cards input in
    let player_cards = update_cards player_cards input in
    play_game ai_cards player_cards prev_cards 1
  else
    let input =
      match Comparison.play ai_cards prev_cards with
      | Skip | Other -> []
      | Continue cards -> cards
    in
    let prev_cards = input in
    let ai_cards = update_ai_cards ai_cards input in
    play_game ai_cards player_cards prev_cards 0
  with _ -> ANSITerminal.print_string [ ANSITerminal.red ] "\nThe format of your input is invalid. Please try again.";
  play_game ai_cards player_cards prev_cards turn

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to Poker.\n\n";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Rules for the game: \n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "The game starts with you, and the turns will go in order You-AI-You-AI...\n\
     Each turn, each player is only allowed to place 0 cards (skip), 1 card, 2 \
     cards, 3 cards, 4 cards or 5 cards. \n\
     Card rank in increasing order: 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K, A, \
     2, 🃟, 🃏\n\
     The suit of cards doesn't matter. \n\
     1 single card can only beat other single cards of a smaller rank. \n\
     2 cards must be a pair of the same rank, and it can only beat other \
     pairs of a smaller rank. \n\
     3 cards must be a triple of the same rank, and it can only beat other triples \
     of a smaller rank. \n\
     Bombs can beat any non-bomb cards. \n\
     A pair of 🃟 🃏 is the most powerful bomb, and 4 cards of the same rank is another \
     type of bomb. \n\
     The player that gets rid of all of their cards first wins!";
  let cards_group = split_in_half scrambled_list in
  let ai_cards = sorted (fst cards_group) in
  let player_cards = sorted (snd cards_group) in
  play_game ai_cards player_cards prev_cards turn

let () = main ()
