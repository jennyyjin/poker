open Poker
open Draw
open Comparison
open Assign
open Play

let cards_group = split_in_half scrambled_list
let ai_cards = sorted (fst cards_group)
let player_cards = sorted (snd cards_group)
let ai_prev = []
let player_prev = []
let begin_board = Draw.print_board ai_cards player_cards ai_prev player_prev
let player_cards = update_cards player_cards [ 1 ]
let second_board = Draw.print_board ai_cards player_cards ai_prev player_prev

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to Poker.\n";
  print_string begin_board;
  print_endline " ";
  print_string second_board
(* print_string (String.concat " " ho); print_endline " "; print_string
   (String.concat " " he); print_endline " "; print_string (String.concat " "
   hu); print_endline " "; print_string (String.concat " " ha) *)

let () = main ()
