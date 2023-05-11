open Comparison
open Helper
open Play

let rec find_two_list_aux (cards : int list) result =
  let fst = double_helper cards in
  match fst with
  | [] -> []
  | [ c1; c2 ] -> result @ [ [ c1; c2 ] ]
  | c1 :: c2 :: t ->
      let new_lst = update_ai_cards cards [ c1; c2 ] in
      find_two_list_aux new_lst (result @ [ [ c1; c2 ] ])
  | _ -> []

let find_two_list cards = find_two_list_aux cards []

let rec find_three_list_aux (cards : int list) result =
  let fst = triple_helper cards in
  match fst with
  | [] -> []
  | [ c1; c2; c3 ] -> result @ [ [ c1; c2; c3 ] ]
  | c1 :: c2 :: c3 :: t ->
      let new_lst = update_ai_cards cards [ c1; c2; c3 ] in
      find_three_list_aux new_lst (result @ [ [ c1; c2; c3 ] ])
  | _ -> []

let find_three_list cards = find_three_list_aux cards []

let find_four (cards : int list) =
  let result = quad cards in
  match result with
  | Other -> []
  | Continue [ c1; c2; c3; c4 ] -> [ c1; c2; c3; c4 ]
  | _ -> []

let rec find_four_list_aux cards result =
  let fst = find_four cards in
  match fst with
  | [] -> []
  | [ c1; c2; c3; c4 ] -> (
      let new_lst = update_ai_cards cards [ c1; c2; c3; c4 ] in
      let snd = find_four new_lst in
      match snd with
      | [] -> result @ [ fst ]
      | _ -> find_four_list_aux new_lst (result @ [ fst ]))
  | _ -> []

let find_four_list cards = find_four_list_aux cards []

let rec remove_duplicates cards =
  match cards with
  | [] -> []
  | [ x ] -> [ x ]
  | x :: y :: rest ->
      if x = y then remove_duplicates (List.filter (( <> ) x) (y :: rest))
      else x :: remove_duplicates (y :: rest)

let rec find_single (cards : int list) =
  let sorted_cards = sorted cards in
  let cards_in_number = List.map (fun x -> number_to_card x) sorted_cards in
  match cards_in_number with
  | [] -> []
  | [ x ] -> [ x ]
  | x :: y :: rest ->
      if x = y then find_single (remove_duplicates (x :: y :: rest))
      else x :: find_single (y :: rest)

let rec find_single_list (lst : int list) = List.map (fun x -> [ x ]) lst

let count_single (cards : int list) =
  let new_lst = find_single cards in
  List.length new_lst

let find_straight (cards : int list) =
  let result = straight_helper cards [ 0; 1; 2; 3; 4 ] in
  match result with
  | Other -> []
  | Continue [ c1; c2; c3; c4; c5 ] -> [ c1; c2; c3; c4; c5 ]
  | _ -> []

let rec find_straight_list_aux cards result =
  let fst = find_straight cards in
  match fst with
  | [] -> []
  | [ c1; _; _; _; _ ] -> (
      let new_lst = update_ai_cards cards [ c1 ] in
      let snd = find_straight new_lst in
      match snd with
      | [] -> result @ [ fst ]
      | _ -> find_straight_list_aux new_lst (result @ [ fst ]))
  | _ -> []

let find_straight_list cards = find_straight_list_aux cards []

let rec find_all_straight_aux (cards : int list) (straight : int list) result =
  match straight with
  | [] -> []
  | [ c1; c2; c3; c4; c5 ] -> (
      let new_cards = update_ai_cards cards [ c1; c2; c3; c4; c5 ] in
      let new_straight = find_straight new_cards in
      match new_straight with
      | [] -> result @ [ [ c1; c2; c3; c4; c5 ] ]
      | [ c6; c7; c8; c9; c10 ] ->
          find_all_straight_aux new_cards [ c6; c7; c8; c9; c10 ]
            (result @ [ [ c1; c2; c3; c4; c5 ] ] @ [ [ c6; c7; c8; c9; 10 ] ])
      | _ -> [])
  | _ -> []

let rec find_all_straight (cards : int list) (straight : int list) =
  find_all_straight_aux cards straight []

let index_of_min lst =
  let rec find_min_idx min_val min_idx cur_idx = function
    | [] -> min_idx
    | x :: rest ->
        if x < min_val then find_min_idx x cur_idx (cur_idx + 1) rest
        else find_min_idx min_val min_idx (cur_idx + 1) rest
  in
  match lst with
  | [] -> None
  | x :: rest -> Some (find_min_idx x 0 1 rest)

let find_best_straight cards straight_lists =
  let group_size = List.length straight_lists in
  match group_size with
  | 0 | 1 -> straight_lists
  | _ -> (
      let new_straight_lists =
        List.map (fun x -> find_all_straight cards x) straight_lists
      in
      let new_rest_cards =
        List.map
          (fun x -> update_ai_cards cards (List.concat x))
          new_straight_lists
      in
      let group_count_unique =
        List.map (fun x -> count_single x) new_rest_cards
      in
      let min = index_of_min group_count_unique in
      match min with
      | None -> straight_lists
      | Some i -> List.nth new_straight_lists i)

(**[split_cards] cards split cards into 5 group:
   [\[quad\];\[straight\];\[triple\];\[double\];\[single\]], note [quad] is a
   list of int list with all choices for quad*)
let split_cards cards =
  let quad = find_four_list cards in
  let result1 = [] @ [ quad ] in
  let quad_cards = List.concat quad in
  let cards1 = update_ai_cards cards quad_cards in
  let straight_helper = find_straight_list cards1 in
  let straight = find_best_straight cards1 straight_helper in
  let result2 = result1 @ [ straight ] in
  let straight_cards = List.concat straight in
  let cards2 = update_ai_cards cards1 straight_cards in
  let triple = find_three_list cards2 in
  let result3 = result2 @ [ triple ] in
  let triple_cards = List.concat triple in
  let cards3 = update_ai_cards cards2 triple_cards in
  let double = find_two_list cards3 in
  let result4 = result3 @ [ double ] in
  let double_cards = List.concat double in
  let cards4 = update_ai_cards cards3 double_cards in
  let single = find_single_list cards4 in
  result4 @ [ single ]

(**[play this other] returns AI's decision based on its current cards and the
   previous card *)
let play (this : int list) (other : int list) : choice =
  let size = List.length other in
  match size with
  | 0 -> Continue [ List.hd this ]
  | 1 -> single this other
  | 2 -> double this other
  | 3 -> triple this other
  | 4 -> quad other
  | 5 ->
      let cardtype = getcardtype other in
      if cardtype = Fullhouse then triple_p_double this other
      else straight_helper this other
  | _ -> Other
