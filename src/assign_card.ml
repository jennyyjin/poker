type card =
  | Spade of int
  | Heart of int
  | Diamond of int
  | Club of int

(** [card_list] is the 54 cards in a poker deck and we name them from 1 to 54 *)
let card_list = List.init 54 (fun i -> i + 1)

(** [scrambled_list] is the 54 cards in a poker deck and we name them from 1 to
    54 and scramble them up*)
let scrambled_list = List.sort (fun _ _ -> Random.int 3 - 1) card_list

(** [split_in_half lst] splits up the cards into two decks.*)
let rec split_in_half lst =
  match lst with
  | [] -> ([], [])
  | [ x ] -> ([ x ], [])
  | x :: y :: rest ->
      let first_half, second_half = split_in_half rest in
      (x :: first_half, y :: second_half)
