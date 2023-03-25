(** [find_index] takes one element from a list, a list and a start index number,
    it runs recursively find the index of the element in the list by starting
    with the start index*)
let rec find_index (element : int) (lst : int list) (index : int) =
  if List.nth lst index = element then index
  else find_index element lst (index + 1)

(** [check_not_index] takes one element from a list, a list and a list of
    indiced number, it returns true if the element's index is on the index_lst,
    false otherwise*)
let rec check_not_index (element : int) (current_lst : int list)
    (index_lst : int list) =
  if List.length index_lst = 0 then true
  else if List.hd index_lst = find_index element current_lst 0 then false
  else check_not_index element current_lst (List.tl index_lst)

(** [update_cards] takes a serial card list that player holds and the cards
    player placed, and update the card list.*)
let update_cards (current_cards : int list) (output_cards : int list) =
  List.filter
    (fun e -> check_not_index e current_cards output_cards)
    current_cards

(** [check_not_index] takes a list and a card serial number, it returns true the
    serial number is not on the list*)
let check_not_card (lst : int list) (card : int) =
  List.for_all (fun e -> e <> card) lst

(** [update_cards] takes a serial card list that player holds and the cards
    player placed, and update the card list.*)
let update_ai_cards (current_cards : int list) (output_cards : int list) =
  List.filter (fun e -> check_not_card output_cards e) current_cards

(** [index_to_num] ... emmmmm forgot what it does...*)
let index_to_num (current_cards : int list) (output_cards : int list) =
  List.map (fun e -> List.nth current_cards e) output_cards
