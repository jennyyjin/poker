(** [find_index element lst index] returns the index of the element in the list *)
let rec find_index (element : int) (lst : int list) (index : int) =
  if List.nth lst index = element then index
  else find_index element lst (index + 1)

(** 忘记了*)
let rec check_not_index (element : int) (current_lst : int list)
    (index_lst : int list) =
  if List.length index_lst = 0 then true
  else if List.hd index_lst = find_index element current_lst 0 then false
  else check_not_index element current_lst (List.tl index_lst)

(** [update_cards current_cards output_cards] returns the rest of the cards when
    some cards are placed down。 This is for player. *)
let update_cards (current_cards : int list) (output_cards : int list) =
  List.filter
    (fun e -> check_not_index e current_cards output_cards)
    current_cards

(** [check_not_card lst card] delete card from lst *)
let check_not_card (lst : int list) (card : int) =
  List.for_all (fun e -> e <> card) lst

(** [update_ai_cards current_cards output_cards] returns the rest of the cards
    when some cards are placed down. This is for AIs *)
let update_ai_cards (current_cards : int list) (output_cards : int list) =
  List.filter (fun e -> check_not_card output_cards e) current_cards

(** [index_to_num current_cards output_cards ] returns the list of cards that is
    placed down by the player based on players index input *)
let index_to_num (current_cards : int list) (output_cards : int list) =
  List.map (fun e -> List.nth current_cards e) output_cards

(**[clear] clear the screen*)
let clear = function
  | x -> Sys.command "clear" + x
