open Poker
open Draw
open Comparison
open Assign
open Play

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
      match Comparison.compare ai_cards prev_cards with
      | Skip | Other -> []
      | Continue cards -> cards
    in
    let prev_cards = input in
    let ai_cards = update_ai_cards ai_cards input in
    play_game ai_cards player_cards prev_cards 0

(* match read_line () with | exception End_of_file -> () | _ -> let player_cards
   = update_cards player_cards (List.map (fun e -> int_of_string e)
   (Str.split_delim (Str.regexp " ") (read_line ()))) in play_game
   player_cards *)

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to Poker.\n";
  (* print_string begin_board; *)
  print_endline " ";
  let cards_group = split_in_half scrambled_list in 
  let ai_cards = sorted (fst cards_group) in 
  let player_cards = sorted (snd cards_group) in 
    play_game ai_cards player_cards prev_cards turn
(* print_string second_board *)
(* print_string (String.concat " " ho); print_endline " "; print_string
   (String.concat " " he); print_endline " "; print_string (String.concat " "
   hu); print_endline " "; print_string (String.concat " " ha) *)

let () = main ()
