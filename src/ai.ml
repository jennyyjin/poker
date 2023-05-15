open Comparison
open Draw
open Play

(** Module Ai implements the functionalities of the two AIs and their
    cooperations. *)

let rec find_two_list_aux (cards : int list) result =
  let fst = dup_list cards in
  match fst with
  | [] -> []
  | [ c1; c2 ] -> result @ [ [ c1; c2 ] ]
  | c1 :: c2 :: t ->
      let new_lst = update_ai_cards cards [ c1; c2 ] in
      find_two_list_aux new_lst (result @ [ [ c1; c2 ] ])
  | _ -> []

let find_two_list cards = find_two_list_aux cards []

let rec find_three_list_aux (cards : int list) result =
  let fst = three_lst cards in
  match fst with
  | [] -> []
  | [ c1; c2; c3 ] -> result @ [ [ c1; c2; c3 ] ]
  | c1 :: c2 :: c3 :: t ->
      let new_lst = update_ai_cards cards [ c1; c2; c3 ] in
      find_three_list_aux new_lst (result @ [ [ c1; c2; c3 ] ])
  | _ -> []

let find_three_list cards = find_three_list_aux cards []

let find_four (cards : int list) =
  let result = find_quad cards in
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
  let result = first_straight cards [ 0; 1; 2; 3; 4 ] in
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

let find_jokers cards =
  let size = List.length cards in
  let joker_cards =
    match size with
    | 0 | 1 -> []
    | _ -> List.filter (fun x -> x = 52 || x = 53) cards
  in
  if List.length joker_cards = 2 then [ joker_cards ] else []

(** [split_cards] cards split cards into 6 group:
    [\[jokers\];\[quad\];\[straight\];\[triple\];\[double\];\[single\]], note
    [quad] is a list of int list with all choices for quad*)
let split_cards cards =
  let jokers = find_jokers cards in
  let result0 = [] @ [ jokers ] in
  let jokers_cards = List.concat jokers in
  let cards0 = update_ai_cards cards jokers_cards in
  let quad = find_four_list cards0 in
  let result1 = result0 @ [ quad ] in
  let quad_cards = List.concat quad in
  let cards1 = update_ai_cards cards0 quad_cards in
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

(** [make_fst_choice cards] is the card that the ai will put down if the user
    skips and they are free to put down any patterns*)
let make_fst_choice (cards : int list) : choice =
  let split = split_cards cards in
  let jokers = List.nth split 0 in
  let quad = List.nth split 1 in
  let straight = List.nth split 2 in
  let triple = List.nth split 3 in
  let double = List.nth split 4 in
  let single = List.nth split 5 in
  let concat1 = straight @ triple @ double @ single in
  let concat2 = triple @ double @ single in
  if List.length quad <> 0 && List.length concat1 = 1 then
    Continue (List.hd quad)
  else if
    List.length quad <> 0
    && List.length triple <> 0
    && List.length concat2 = 2
    && List.hd concat2 = List.hd triple
    && concat2 <> triple
  then Continue (List.hd quad)
  else if List.length jokers <> 0 && List.length concat1 = 1 then
    Continue (List.hd jokers)
  else if
    List.length jokers <> 0
    && List.length triple <> 0
    && List.length concat2 = 2
    && List.hd concat2 = List.hd triple
    && concat2 <> triple
  then Continue (List.hd jokers)
  else if List.length straight <> 0 then Continue (List.hd straight)
  else if List.length triple <> 0 && List.length single <> 0 then
    Continue (List.hd triple @ List.hd single)
  else if List.length single <> 0 then Continue (List.hd single)
  else if List.length triple <> 0 && List.length double <> 0 then
    Continue (List.hd triple @ List.hd double)
  else if List.length triple <> 0 then Continue (List.hd triple)
  else if List.length double <> 0 then Continue (List.hd double)
  else if List.length quad <> 0 then Continue (List.hd quad)
  else if List.length jokers <> 0 && List.hd jokers = cards then
    Continue (List.hd jokers)
  else Continue [ List.hd cards ]

(** [check_quad cards] returns true if the bomb is J or greater and false
    otherwise*)
let check_quad (cards : int list list) : bool =
  match cards with
  | [ [ c1; c2; c3; c4 ] ] -> c1 mod 13 > 7
  | _ -> failwith "wrong pattern"

(** [check_triple cards] returns true if the triple is J or greater and false
    otherwise *)
let check_triple (cards : int list list) : bool =
  match cards with
  | [ [ c1; c2; c3 ] ] -> c1 mod 13 > 7
  | _ -> failwith "wrong pattern"

(** [check_double cards] returns true if the double is K or greater and false
    otherwise *)
let check_double (cards : int list list) : bool =
  match cards with
  | [ [ c1; c2 ] ] -> c1 mod 13 > 9
  | _ -> failwith "wrong pattern"

(** [check_single card] returns true if the card is A or greater and false
    otherwise *)
let check_single (card : int list list) : bool =
  match card with
  | [ [ c ] ] -> c >= 52 || c mod 13 > 9
  | _ -> failwith "wrong pattern"

(** [check_triple_single cards] returns true if Triple with Single has a rank of
    J or greater false otherwise *)
let check_triple_single (cards : int list list) : bool =
  let cards_format =
    match cards with
    | [ format ] -> format
    | _ -> failwith "invalid format"
  in
  match sorted cards_format with
  | [ c1; c2; c3; c4 ] ->
      if compare_card c1 c2 = 0 then c1 mod 13 > 7 else c2 mod 13 > 7
  | _ -> failwith "wrong pattern"

(** [check_triple_double cards] returns true if Triple with Double has a rank of
    J or greater false otherwise *)
let check_triple_double (cards : int list list) : bool =
  let cards_format =
    match cards with
    | [ format ] -> format
    | _ -> failwith "invalid format"
  in
  match sorted cards_format with
  | [ c1; c2; c3; c4; c5 ] ->
      if compare_card c2 c3 = 0 then c1 mod 13 > 7 else c3 mod 13 > 7
  | _ -> failwith "wrong pattern"

(** [check_straight cards] returns true if the straight is [10; J; Q; K; A] and
    false otherwise *)
let check_straight (cards : int list list) : bool =
  let cards_format =
    match cards with
    | [ format ] -> format
    | _ -> failwith "invalid format"
  in
  match sorted cards_format with
  | [ c1; c2; c3; c4; c5 ] -> c1 mod 13 >= 7
  | _ -> failwith "wrong pattern"

(** [collab prev_cards] returns true if previous cards are large according to
    the check functions defined above and false otherwise *)
let collab (prev_cards : int list) : bool =
  let card_type = getcardtype prev_cards in
  match card_type with
  | Bomb -> check_quad [ prev_cards ]
  | Straight -> check_straight [ prev_cards ]
  | Double -> check_double [ prev_cards ]
  | Single -> check_single [ prev_cards ]
  | Triple -> check_triple [ prev_cards ]
  | TripleOne -> check_triple_single [ prev_cards ]
  | Fullhouse -> check_triple_double [ prev_cards ]
  | _ -> false

(** [play this other] returns AI's decision based on its current cards and the
    previous card *)
let play_helper (this : int list) (other : int list) : choice =
  let splitted_cards = split_cards this in
  let size = List.length other in
  match size with
  | 0 -> make_fst_choice this
  | 1 -> (
      let result = single (List.flatten (List.nth splitted_cards 5)) other in
      match result with
      | Continue [ c1 ] -> Continue [ c1 ]
      | _ -> single (List.flatten (List.nth splitted_cards 3)) other)
  | 2 ->
      let not_joker = List.hd other <> 52 && List.hd other <> 53 in
      if not_joker then
        let result = double (List.flatten (List.nth splitted_cards 4)) other in
        match result with
        | Continue [ c1; c2 ] -> Continue [ c1; c2 ]
        | _ -> double (List.flatten (List.nth splitted_cards 3)) other
      else Other
  | 3 -> triple (List.flatten (List.nth splitted_cards 3)) other
  | 4 ->
      let cardtype = getcardtype other in
      if cardtype = TripleOne then
        triple_p_one
          (List.flatten (List.nth splitted_cards 3)
          @ List.flatten (List.nth splitted_cards 5))
          other
      else quad this other
  | 5 ->
      let cardtype = getcardtype other in
      if cardtype = Fullhouse then
        triple_p_double
          (List.flatten (List.nth splitted_cards 3)
          @ List.flatten (List.nth splitted_cards 4))
          other
      else straight (List.flatten (List.nth splitted_cards 2)) other
  | _ -> Other

let play (this : int list) (other : int list) (player_cards : int list) : choice
    =
  let cards_group = split_cards this in
  let new_lst = List.filter (fun x -> List.length x <> 0) cards_group in
  match new_lst with
  | [ [ fst ]; [ snd ] ] ->
      if getcardtype fst = Joker then Continue fst
      else if getcardtype fst = Bomb then quad this other
      else play_helper this other
  | _ -> (
      let result = play_helper this other in
      match result with
      | Continue [ _ ]
      | Continue [ _; _ ]
      | Continue [ _; _; _ ]
      | Continue [ _; _; _; _ ]
      | Continue [ _; _; _; _; _ ] -> result
      | _ -> (
          let size = List.length player_cards in
          let other_type = getcardtype other in
          if size < 3 then quad this other
          else
            match other_type with
            | Single | Double -> Other
            | _ -> if size < 10 then quad this other else Other))
