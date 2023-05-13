(** Module Assign represent the assignments of cards in poker*)

(** [card_list ()] is a funciton that produce a list of integer from 0 to 53
    inclusive*)
let card_list () = List.init 54 (fun i -> i)

(** [random n] reinitialize the ramdom number generator and generate a random
    number between 0 and n*)
let random n =
  Random.self_init ();
  Random.int n

(** [scrambled_list ()] is the 54 cards in a poker deck and we name them from 0
    to 53 and scramble them up*)
let scrambled_list () = List.sort (fun _ _ -> random 3 - 1) (card_list ())

(** [split_in_three lst] splits up the cards into three decks in which the first
    person gets the card if there is only one card. The first person and second
    person gets one card if there is two cards *)
let rec split_in_three lst =
  match lst with
  | [] -> ([], [], [])
  | [ x ] -> ([ x ], [], [])
  | [ x; y ] -> ([ x ], [ y ], [])
  | [ x; y; z ] -> ([], [], [ x; y; z ])
  | x :: y :: z :: rest ->
      let first, second, third = split_in_three rest in
      (x :: first, y :: second, z :: third)
