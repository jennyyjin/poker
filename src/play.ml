let rec find_index (element : int) (lst : int list) (index : int) =
  if List.nth lst index = element then index
  else find_index element lst (index + 1)

let rec check_not_index (element : int) (current_lst : int list)
    (index_lst : int list) =
  if List.length index_lst = 0 then true
  else if List.hd index_lst = find_index element current_lst 0 then false
  else check_not_index element current_lst (List.tl index_lst)

(*input: current_cards is this player's current_cards\\ output_cards is the
  indexes of the output cards *)
let update_cards (current_cards : int list) (output_cards : int list) =
  List.filter
    (fun e -> check_not_index e current_cards output_cards)
    current_cards

let check_not_card (lst : int list) (card : int) =
  List.for_all (fun e -> e <> card) lst

let update_ai_cards (current_cards : int list) (output_cards : int list) =
  List.filter (fun e -> check_not_card output_cards e) current_cards

let index_to_num (current_cards : int list) (output_cards : int list) =
  List.map (fun e -> List.nth current_cards e) output_cards
