open Poker
open Draw
open Comparison

let hi = Draw.print_cards [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13 ]
let ho = Draw.print_cards [ 14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 26 ]
let he = Draw.print_cards [ 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39 ]
let hu = Draw.print_cards [ 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52 ]
let ha = Draw.print_cards [ 53; 54 ]

let main () =
  print_string (String.concat " " hi);
  print_endline " ";
  print_string (String.concat " " ho);
  print_endline " ";
  print_string (String.concat " " he);
  print_endline " ";
  print_string (String.concat " " hu);
  print_endline " ";
  print_string (String.concat " " ha)

let () = main ()
