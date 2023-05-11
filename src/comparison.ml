open Assign
open Play
open Helper

exception Empty
exception Wrong

type choice =
  | Continue of int list
  | Skip
  | Other

type compare =
  | EQ
  | LT
  | GT
  | IV

type cardstype =
  | Single
  | Double
  | Triple
  | Fullhouse
  | Straight
  | Bomb
  | Joker
  | Invalid
  | Empty

(** [compare_card x y] is a comparison function that output either -1, 0 or 1 to
    denote the relative rank between cards. So we denote 0 and anything mod 13
    to 0 as the samllest rank of the deck, and 53 as the largest Joker in the
    deck *)
let compare_card x y =
  if x = 53 then 1
  else if y = 53 then -1
  else if x = 52 then 1
  else if y = 52 then -1
  else
    let x_num = x mod 13 in
    let y_num = y mod 13 in
    if x_num < y_num then -1 else if x_num > y_num then 1 else 0

let sorted (cards : int list) : int list = List.sort compare_card cards

let sorted_uniq (cards : int list) : int list =
  List.sort_uniq compare_card cards

let rec single (this : int list) (other : int list) : choice =
  match this with
  | [] -> Other
  | h :: t ->
      if compare_card h (List.hd other) = 1 then Continue [ h ]
      else single t other

let rec remove_joker (lst : int list) =
  match lst with
  | [] -> []
  | h :: t -> if h = 52 || h = 53 then remove_joker t else h :: remove_joker t

let int_list_to_string (lst : int list) : string =
  let str_list = List.map string_of_int (sorted_uniq lst) in
  String.concat "" str_list

let char_to_int (c : char) : int = int_of_char c - int_of_char '0'

let rec straight (this : int list) (other : int list) : choice =
  match sorted_uniq (remove_joker this) with
  | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] | [ _; _; _; _ ] -> Other
  | c1 :: c2 :: c3 :: c4 :: c5 :: t ->
      if
        (c1 mod 13) - (char_to_int (int_list_to_string other).[0] mod 13)
        = (c2 mod 13) - (char_to_int (int_list_to_string other).[1] mod 13)
        && (c2 mod 13) - (char_to_int (int_list_to_string other).[1] mod 13)
           = (c3 mod 13) - (char_to_int (int_list_to_string other).[2] mod 13)
        && (c3 mod 13) - (char_to_int (int_list_to_string other).[2] mod 13)
           = (c4 mod 13) - (char_to_int (int_list_to_string other).[3] mod 13)
        && (c4 mod 13) - (char_to_int (int_list_to_string other).[3] mod 13)
           = (c5 mod 13) - (char_to_int (int_list_to_string other).[4] mod 13)
      then Continue [ c1; c2; c3; c4; c5 ]
      else straight (c2 :: c3 :: c4 :: c5 :: t) other

let rec double_helper (lst : int list) : int list =
  match sorted (remove_joker lst) with
  | [] | [ _ ] -> []
  | c1 :: c2 :: t ->
      if c1 mod 13 = c2 mod 13 then c1 :: c2 :: double_helper t
      else double_helper (c2 :: t)

let rec double_lst (this : int list) (other : int list) : int list =
  match double_helper this with
  | [] | [ _ ] -> []
  | c1 :: c2 :: t ->
      if compare_card c1 (List.hd other) = 1 then [ c1; c2 ]
      else double_lst t other

let double (this : int list) (other : int list) : choice =
  assert (
    match other with
    | [ x; y ] -> compare_card x y = 0
    | _ -> false);
  match double_lst this other with
  | [] -> Other
  | [ c1; c2 ] -> Continue [ c1; c2 ]
  | _ -> raise Wrong

let rec triple_helper (lst : int list) : int list =
  match sorted (remove_joker lst) with
  | [] | [ _ ] | [ _; _ ] -> []
  | c1 :: c2 :: c3 :: t ->
      if c1 mod 13 = c2 mod 13 && c2 mod 13 = c3 mod 13 then
        [ c1; c2; c3 ] @ triple_helper t
      else triple_helper (c2 :: c3 :: t)

let rec triple_lst (this : int list) (other : int list) : int list =
  match triple_helper this with
  | [] | [ _ ] | [ _; _ ] -> []
  | c1 :: c2 :: c3 :: t ->
      if compare_card c1 (List.hd other) = 1 then [ c1; c2; c3 ]
      else triple_lst t other

let triple (this : int list) (other : int list) : choice =
  assert (
    match other with
    | [ x; y; z ] -> compare_card x y = 0 && compare_card y z = 0
    | _ -> false);
  match triple_lst this other with
  | [] -> Other
  | [ c1; c2; c3 ] -> Continue [ c1; c2; c3 ]
  | _ -> raise Wrong

let rec quad (lst : int list) : choice =
  match sorted (remove_joker lst) with
  | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] -> Other
  | c1 :: c2 :: c3 :: c4 :: t ->
      if c1 mod 13 = c2 mod 13 && c2 mod 13 = c3 mod 13 && c3 mod 13 = c4 mod 13
      then Continue [ c1; c2; c3; c4 ]
      else quad (c2 :: c3 :: c4 :: t)

let rec joker_pair (lst : int list) : choice =
  match sorted lst with
  | [] | [ _ ] -> Skip
  | _ :: [ 52; 53 ] -> Continue [ 52; 53 ]
  | _ -> Skip

let rec triple_p_double (this : int list) (other : int list) : choice =
  let t_cards = [ List.nth other 0; List.nth other 1; List.nth other 2 ] in
  match triple this t_cards with
  | Skip -> Other
  | Other -> Other
  | Continue cards -> (
      let rest_cards = update_ai_cards this cards in
      match double_helper rest_cards with
      | [] | [ _ ] -> Other
      | d1 :: d2 :: t -> Continue (cards @ [ d1 ] @ [ d2 ]))

let getcardtype (this : int list) : cardstype =
  let sort_this = sorted this in
  let size = List.length sort_this in
  match size with
  | 0 -> Empty
  | 1 -> Single
  | 2 ->
      let card1 = Helper.number_to_card (List.nth sort_this 0) in
      let card2 = Helper.number_to_card (List.nth sort_this 1) in
      if card1 = 0 && card2 = 0 then Joker
      else if card1 = card2 then Double
      else Invalid
  | 3 ->
      let card1 = Helper.number_to_card (List.nth sort_this 0) in
      let card2 = Helper.number_to_card (List.nth sort_this 1) in
      let card3 = Helper.number_to_card (List.nth sort_this 2) in
      if card1 = card2 && card2 = card3 then Triple else Invalid
  | 4 ->
      let card1 = Helper.number_to_card (List.nth sort_this 0) in
      let card2 = Helper.number_to_card (List.nth sort_this 1) in
      let card3 = Helper.number_to_card (List.nth sort_this 2) in
      let card4 = Helper.number_to_card (List.nth sort_this 3) in
      if card1 = card2 && card2 = card3 && card3 = card4 then Bomb else Invalid
  | 5 ->
      let card1 = Helper.number_to_card (List.nth sort_this 0) in
      let card2 = Helper.number_to_card (List.nth sort_this 1) in
      let card3 = Helper.number_to_card (List.nth sort_this 2) in
      let card4 = Helper.number_to_card (List.nth sort_this 3) in
      let card5 = Helper.number_to_card (List.nth sort_this 4) in
      if card1 = card2 && card2 = card3 && card4 = card5 then Fullhouse
      else if
        card4 <> 0 && card5 <> 0 && card1 <> 1 && card1 <> 2 && card1 <> 11
        && card1 <> 12 && card1 <> 13
      then
        if
          card2 = card1 + 1
          && card3 = card2 + 1
          && card4 = card3 + 1
          && card5 = card4 + 1
        then Straight
        else if
          card1 = 10 && card2 = 11 && card3 = 12 && card4 = 13 && card5 = 1
        then Straight
        else Invalid
      else Invalid
  | _ -> Invalid

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
      else straight this other
  | _ -> Other

let check_same_type (this : int list) (other : int list) =
  let cards1 = getcardtype this in
  let cards2 = getcardtype other in
  if cards1 = cards2 && cards1 != Invalid && cards2 != Invalid then true
  else false

let compare_same_type (this : int list) (other : int list) =
  let cardstype = getcardtype this in
  match cardstype with
  | Single | Double | Triple | Straight | Bomb ->
      let diff = compare_card (List.hd this) (List.hd other) in
      if diff = -1 then LT else if diff = 1 then GT else EQ
  | Fullhouse ->
      let diff3 = compare_card (List.hd this) (List.hd other) in
      let diff2 = compare_card (List.nth this 4) (List.nth other 4) in
      if diff3 = -1 then LT
      else if diff3 = 1 then GT
      else if diff2 = -1 then LT
      else if diff2 = 1 then GT
      else EQ
  | Empty -> GT
  | _ -> IV

let compare_diff_type (this : int list) (other : int list) =
  let cardstype = getcardtype this in
  let cardstypeother = getcardtype other in
  if cardstypeother = Empty && cardstype != Invalid then GT
  else
    match cardstype with
    | Single | Double | Triple | Straight | Fullhouse -> IV
    | Bomb -> if getcardtype other = Joker then LT else GT
    | Joker -> GT
    | Empty -> GT
    | Invalid -> IV

let check_valid (cards1 : int list) (cards2 : int list) =
  let samecard = check_same_type cards1 cards2 in
  match samecard with
  | true ->
      let x = print_endline "true" in
      let diff = compare_same_type cards1 cards2 in
      if diff = GT then true else false
  | false ->
      let y = print_endline "false" in
      let diff = compare_diff_type cards1 cards2 in
      if diff = GT then true else false
