open Play

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
  | TripleOne
  | Fullhouse
  | Straight
  | Bomb
  | Joker
  | Invalid
  | Empty

let compare_card x y =
  if x = 53 then 1
  else if y = 53 then -1
  else if x = 52 then 1
  else if y = 52 then -1
  else
    let x_num = x mod 13 in
    let y_num = y mod 13 in
    if x_num < y_num then -1 else if x_num > y_num then 1 else 0

(** [sorted cards] sorts the list of cards in ascending order based on card rank *)
let sorted (cards : int list) : int list = List.sort compare_card cards

(** [sorted_uniq cards] sorts the list of cards in ascending order based on card
    rank without duplicates *)
let sorted_uniq (cards : int list) : int list =
  List.sort_uniq compare_card cards

(** [remove_joker lst] returns a list of cards with jokers removed *)
let rec remove_joker (lst : int list) =
  match lst with
  | [] -> []
  | h :: t -> if h = 52 || h = 53 then remove_joker t else h :: remove_joker t

(**[getcardtype this] returns this's card type *)
let getcardtype (this : int list) : cardstype =
  let sort_this = sorted this in
  let size = List.length sort_this in
  match size with
  | 0 -> Empty
  | 1 -> Single
  | 2 ->
      let card1 = Draw.number_to_card (List.nth sort_this 0) in
      let card2 = Draw.number_to_card (List.nth sort_this 1) in
      if card1 = 0 && card2 = 0 then Joker
      else if card1 = card2 then Double
      else Invalid
  | 3 ->
      let card1 = Draw.number_to_card (List.nth sort_this 0) in
      let card2 = Draw.number_to_card (List.nth sort_this 1) in
      let card3 = Draw.number_to_card (List.nth sort_this 2) in
      if card1 = card2 && card2 = card3 then Triple else Invalid
  | 4 ->
      let card1 = Draw.number_to_card (List.nth sort_this 0) in
      let card2 = Draw.number_to_card (List.nth sort_this 1) in
      let card3 = Draw.number_to_card (List.nth sort_this 2) in
      let card4 = Draw.number_to_card (List.nth sort_this 3) in
      if card1 = card2 && card2 = card3 && card3 = card4 then Bomb
      else if card1 = card2 && card2 = card3 then TripleOne
      else if card2 = card3 && card3 = card4 then TripleOne
      else Invalid
  | 5 ->
      let card1 = Draw.number_to_card (List.nth sort_this 0) in
      let card2 = Draw.number_to_card (List.nth sort_this 1) in
      let card3 = Draw.number_to_card (List.nth sort_this 2) in
      let card4 = Draw.number_to_card (List.nth sort_this 3) in
      let card5 = Draw.number_to_card (List.nth sort_this 4) in
      if card1 = card2 && card2 = card3 && card4 = card5 then Fullhouse
      else if card1 = card2 && card3 = card4 && card4 = card5 then Fullhouse
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

(** [single this other] returns a single card to put down in response to the
    single card the other player just put down. Returns [Continue card] where
    card is the list of card to put down if there is a card in [this] with a
    greater rank than the opponent's card and [Other] if there are no cards
    greater. Requires: [this] is not empty, and [other] contains only one
    integer *)
let rec single (this : int list) (other : int list) : choice =
  match this with
  | [] -> Other
  | h :: t -> (
      match other with
      | [] -> Continue [ List.hd this ]
      | _ ->
          if compare_card h (List.hd other) = 1 then Continue [ h ]
          else single t other)

(** [int_list_to_string lst] converts a list of integers to a string in
    ascending order without duplicates *)
let int_list_to_string (lst : int list) : string =
  let str_list = List.map string_of_int (sorted_uniq lst) in
  String.concat "" str_list

(** [char_to_int c] converts a char to an int *)
let char_to_int (c : char) : int = int_of_char c - int_of_char '0'

(** [remove_joker_two lst] returns a list of cards with jokers and 2 removed *)
let rec remove_joker_two (lst : int list) =
  let remove_joker_lst = remove_joker lst in
  match remove_joker_lst with
  | [] -> []
  | h :: t ->
      if h = 12 || h = 25 || h = 38 || h = 51 then remove_joker_two t
      else h :: remove_joker_two t

(** [first_straight this other] returns the first straight found on card list
    this *)
let rec first_straight (this : int list) (other : int list) : choice =
  match sorted_uniq (remove_joker_two this) with
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
      else first_straight (c2 :: c3 :: c4 :: c5 :: t) other

(** [straight this other] returns a straight to put down in response to the
    straight the other player just put down. Returns [Continue card] where card
    is the list of cards to put down if there is a straight in [this] with
    greater ranks than [other] and [Other] if there are no cards greater.
    Requires: [this] is not empty, and [other] is a list of five consecutive
    numbers with no duplicates *)
let rec straight (this : int list) (other : int list) : choice =
  let smallest = [ 0; 1; 2; 3; 4 ] in
  let fst = first_straight this smallest in
  match fst with
  | Other -> Other
  | Continue [ c1; c2; c3; c4; c5 ] -> (
      let diff =
        compare_card
          (List.hd (sorted [ c1; c2; c3; c4; c5 ]))
          (List.hd (sorted other))
      in
      if diff = 1 then Continue [ c1; c2; c3; c4; c5 ]
      else
        match this with
        | h :: t -> straight t other
        | [] -> Other)
  | _ -> Other

(** [dup_list lst] only contains duplicates *)
let rec dup_list (lst : int list) : int list =
  match sorted (remove_joker lst) with
  | [] | [ _ ] -> []
  | c1 :: c2 :: t ->
      if c1 mod 13 = c2 mod 13 then c1 :: c2 :: dup_list t
      else dup_list (c2 :: t)

(** [double_lst this other] returns a list of a pair of cards to put down if
    there is a pair in [this] greater than [other] and returns an empty list
    otherwise *)
let rec double_lst (this : int list) (other : int list) : int list =
  match dup_list this with
  | [] | [ _ ] -> []
  | c1 :: c2 :: t ->
      if compare_card c1 (List.hd other) = 1 then [ c1; c2 ]
      else double_lst t other

(** [double this other] returns [Continue card] where card is a pair of the same
    rank if there is a pair in [this] greater than [other] and [Other] otherwise *)
let double (this : int list) (other : int list) : choice =
  assert (
    match other with
    | [ x; y ] -> compare_card x y = 0
    | _ -> false);
  match double_lst this other with
  | [] -> Other
  | [ c1; c2 ] -> Continue [ c1; c2 ]
  | _ -> raise Wrong

(** [threes_lst lst] only contains triples *)
let rec three_lst (lst : int list) : int list =
  match sorted (remove_joker lst) with
  | [] | [ _ ] | [ _; _ ] -> []
  | c1 :: c2 :: c3 :: t ->
      if c1 mod 13 = c2 mod 13 && c2 mod 13 = c3 mod 13 then
        [ c1; c2; c3 ] @ three_lst t
      else three_lst (c2 :: c3 :: t)

(** [triple_lst this other] returns a list of a triple to put down if there is a
    triple in [this] greater than [other] and returns an empty list otherwise *)
let rec triple_lst (this : int list) (other : int list) : int list =
  match three_lst this with
  | [] | [ _ ] | [ _; _ ] -> []
  | c1 :: c2 :: c3 :: t ->
      if compare_card c1 (List.hd other) = 1 then [ c1; c2; c3 ]
      else triple_lst t other

(** [triple this other] returns [Continue card] where card is a triple of the
    same rank if there is a triple in [this] greater than [other] and [Other]
    otherwise *)
let triple (this : int list) (other : int list) : choice =
  assert (
    match other with
    | [ x; y; z ] -> compare_card x y = 0 && compare_card y z = 0
    | _ -> false);
  match triple_lst this other with
  | [] -> Other
  | [ c1; c2; c3 ] -> Continue [ c1; c2; c3 ]
  | _ -> raise Wrong

(** [find_quad lst] returns [Continue card] where card is a list of four cards
    of the same rank if there is a four-of-a-kind in [lst] and [Other] otherwise *)
let rec find_quad (lst : int list) : choice =
  match sorted (remove_joker lst) with
  | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] -> Other
  | c1 :: c2 :: c3 :: c4 :: t ->
      if c1 mod 13 = c2 mod 13 && c2 mod 13 = c3 mod 13 && c3 mod 13 = c4 mod 13
      then Continue [ c1; c2; c3; c4 ]
      else find_quad (c2 :: c3 :: c4 :: t)

(** [quad_lst] returns [Continue card] where card is a list of four cards of the
    same rank if there is a four-of-a-kind in [lst] and [Other] otherwise *)
let rec quad (lst : int list) (other : int list) : choice =
  let result = find_quad lst in
  match result with
  | Other -> Other
  | Continue [ c1; c2; c3; c4 ] ->
      if getcardtype other = Joker then Other
      else if getcardtype other = Bomb then
        let diff = compare_card c1 (List.hd other) in
        if diff = 1 then result
        else
          let new_cards = update_ai_cards lst [ c1; c2; c3; c4 ] in
          quad new_cards other
      else result
  | _ -> Other

(** [joker_pair lst] returns [Continue card] where card is a list of two jokers
    if there are two jokers in [lst] and [Skip] otherwise *)
let rec joker_pair (lst : int list) : choice =
  match sorted lst with
  | [] | [ _ ] -> Skip
  | _ :: [ 52; 53 ] -> Continue [ 52; 53 ]
  | _ -> Skip

(** [triple_p_one this other] returns [Continue card] where card is a list of
    four cards of the same rank if there is a one-of-a-kind in [lst] and [Other]
    otherwise *)
let rec triple_p_one (this : int list) (other : int list) : choice =
  let t_cards = three_lst other in
  match triple this t_cards with
  | Skip -> Other
  | Other -> Other
  | Continue cards -> (
      let rest_cards = update_ai_cards this cards in
      let s_card = single rest_cards [] in
      match s_card with
      | Other -> Other
      | Continue [ i ] -> Continue (cards @ [ List.hd rest_cards ])
      | _ -> Other)

(** [triple_p_double this other] returns [Continue card] where card is a list of
    four cards of the same rank if there is a two-of-a-kind in [lst] and [Other]
    otherwise *)
let rec triple_p_double (this : int list) (other : int list) : choice =
  let t_cards = three_lst other in
  match triple this t_cards with
  | Skip -> Other
  | Other -> Other
  | Continue cards -> (
      let rest_cards = update_ai_cards this cards in
      match dup_list rest_cards with
      | [] | [ _ ] -> Other
      | d1 :: d2 :: t -> Continue (cards @ [ d1 ] @ [ d2 ]))

(**[check_same_type this other] returns true if this and that have the same card
   type *)
let check_same_type (this : int list) (other : int list) =
  let cards1 = getcardtype this in
  let cards2 = getcardtype other in
  if cards1 = cards2 && cards1 != Invalid && cards2 != Invalid then true
  else false

(**[compare_same_type this other] returns true if this is greater than that
   Precondition: this and that have the same card type *)
let compare_same_type (this : int list) (other : int list) =
  let cardstype = getcardtype this in
  match cardstype with
  | Single | Double | Triple | Straight | Bomb ->
      let diff = compare_card (List.hd this) (List.hd other) in
      if diff = -1 then LT else if diff = 1 then GT else EQ
  | Fullhouse | TripleOne ->
      let diff3 = compare_card (List.hd this) (List.hd other) in
      let diff2 = compare_card (List.nth this 3) (List.nth other 3) in
      if diff3 = -1 then LT
      else if diff3 = 1 then GT
      else if diff2 = -1 then LT
      else if diff2 = 1 then GT
      else EQ
  | Empty -> GT
  | _ -> IV

(**[compare_diff_type this other] returns true if this is greater than that
   Precondition: this and that have different card types *)
let compare_diff_type (this : int list) (other : int list) =
  let cardstype = getcardtype this in
  let cardstypeother = getcardtype other in
  if cardstypeother = Empty && cardstype != Invalid then GT
  else
    match cardstype with
    | Single | Double | Triple | Straight | Fullhouse | TripleOne -> IV
    | Bomb -> if getcardtype other = Joker then LT else GT
    | Joker -> GT
    | Empty -> GT
    | Invalid -> IV

(**[check_valid card1 card2] returns true if c1 is greater than c2*)
let check_valid (cards1 : int list) (cards2 : int list) =
  let samecard = check_same_type cards1 cards2 in
  match samecard with
  | true ->
      let diff = compare_same_type cards1 cards2 in
      if diff = GT then true else false
  | false ->
      let diff = compare_diff_type cards1 cards2 in
      if diff = GT then true else false
