open OUnit2
open Poker
open Draw
open Comparison
open Assign
open Play
open Ai

(** Our testing plan involves both automatic and manual testing methods to
    ensure the correctness of the program. We automatically test the deck
    patterns, split card features, and deck types through the program. For core
    modules such as assign, comparison, and play, we extensively test them
    through glass box testing.

    We manually test the program's executables by interacting with the game
    through the make run feature. Specifically, we test some functions in module
    assign and nearly all functions in module play through manual testing using
    make run. This is because module assign contains randomized shuffling
    functions and module draw contains the drawings needed for the games on the
    terminal.

    Our testing plan demonstrates a systematic approach to ensure the
    correctness of our program. We prioritize checking the card patterns and
    split functions to ensure that both users and AIs have the right amount of
    decks and putting down the right deck at a minimum.*)

(** [string_of_int_pair lst1 lst2] is the printer functions that print out the
    result of the split_in_half function that helps with debugging *)
let string_of_int_pair (lst1, lst2, lst3) =
  let rec string_of_int_list lst =
    match lst with
    | [] -> ""
    | hd :: tl -> string_of_int hd ^ " " ^ string_of_int_list tl
  in
  "(" ^ string_of_int_list lst1 ^ ", " ^ string_of_int_list lst2 ^ ","
  ^ string_of_int_list lst3 ^ ")"

(** [split_in_half_test] name card_list expected_ouput asserts the corectness
    output of split_in_half *)
let split_in_half_test (name : string) (card_list : 'a list)
    (expected_output : 'a list * 'a list * 'a list) : test =
  name >:: fun _ ->
  assert_equal (split_in_three card_list) expected_output
    ~printer:string_of_int_pair

(** [card_list1] is a list of integers from 0 to 53 inclusive that use to
    represent the 54 cards in poker *)
let card_lst = List.init 54 (fun i -> i)

(** [card_fst] is a list of even integers from 0 to 53 inclusive that use to
    represent the card taken by user 1 *)
let card_fst =
  [ 0; 3; 6; 9; 12; 15; 18; 21; 24; 27; 30; 33; 36; 39; 42; 45; 48 ]

(** [card_snd] is a list of odd integers from 0 to 53 inclusive that use to
    represent the card taken by user 2 *)
let card_snd =
  [ 1; 4; 7; 10; 13; 16; 19; 22; 25; 28; 31; 34; 37; 40; 43; 46; 49 ]

(** [card_third] is a list of odd integers from 0 to 53 inclusive that use to
    represent the card taken by user 3 *)
let card_third =
  [
    2; 5; 8; 11; 14; 17; 20; 23; 26; 29; 32; 35; 38; 41; 44; 47; 50; 51; 52; 53;
  ]

let split_in_half_tests =
  [
    split_in_half_test
      "Split no card should not have cards for the first person nor the second \
       person nor the third"
      [] ([], [], []);
    split_in_half_test
      "Split one card should give the fst the first card and snd the second no \
       card and third no card"
      [ 0 ] ([ 0 ], [], []);
    split_in_half_test
      "Split two cards should give the fst the first card and snd the second \
       card and third no card"
      [ 0; 1 ] ([ 0 ], [ 1 ], []);
    split_in_half_test
      "Split two cards should give the fst the first card and snd the second \
       card if the order has been change"
      [ 1; 0 ] ([ 1 ], [ 0 ], []);
    split_in_half_test "Split the whole deck of poker in 3" card_lst
      (card_fst, card_snd, card_third);
  ]

let assign_card_tests =
  [ "test suite for split card" >::: List.flatten [ split_in_half_tests ] ]

(** [compare_card_test name card1 card2 expected_output] ensures the correctness
    of the basic compare function in the system name compare *)
let compare_card_test (name : string) (card1 : int) (card2 : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal (compare_card card1 card2) expected_output ~printer:string_of_int

let compare_card_tests =
  [
    compare_card_test "compare_card 1 2 is -1" 1 2 ~-1;
    compare_card_test "compare_card 1 14 is 0 " 1 14 0;
    compare_card_test "compare_card 4 14 is 1" 4 14 1;
    compare_card_test "compare_card 10 14 is -1" 10 14 1;
    compare_card_test "compare_card 13 26 is 0 " 13 26 0;
    compare_card_test "compare_card 52 53 is 1" 52 53 ~-1;
    compare_card_test "compare_card 0 39 is 0" 0 39 0;
  ]

(** [single_test name card1 card2 expected_output] ensures the correctness of
    the single card that can be put down by the player *)
let single_test (name : string) (card1 : int list) (card2 : int list)
    (expected_output : choice) : test =
  name >:: fun _ -> assert_equal (single card1 card2) expected_output

let single_tests =
  [
    single_test
      "When you have smaller deck than then oppoents, you cannot return a card "
      [ 1 ] [ 4 ] Other;
    single_test
      "When you have smaller deck than then oppoents, you cannot return a card "
      [ 1; 2; 3 ] [ 4 ] Other;
    single_test
      "When you have a deck greater than the oppoents, you can continue with \
       that deck"
      [ 4; 5; 6 ] [ 1 ] (Continue [ 4 ]);
    single_test
      "When you have a smaller deck than the other put down, you are unable to \
       continue because the mod 13 value of each is less than the card puyt \
       down by other"
      [ 0; 13; 26 ] [ 4 ] Other;
    single_test
      "When you have a bigger deck than the card other put down, you can \
       simply return that earliest deck"
      [ 14; 12; 13 ] [ 24 ] (Continue [ 12 ]);
  ]

(** [double_test name card1 card2 expected_output] ensures the correctness of
    the double card that can be put down by the player *)
let double_test (name : string) (card1 : int list) (card2 : int list)
    (expected_output : choice) : test =
  name >:: fun _ -> assert_equal (double card1 card2) expected_output

let double_tests =
  [
    double_test
      "When you dont have enough card, ie. no card, you may simply skip" []
      [ 0; 13 ] Other;
    double_test
      "When you dont have enough card, ie. one card, you may simply skip" [ 10 ]
      [ 0; 13 ] Other;
    double_test
      "When you do have enough card, but not a greater deck than the other \
       play, you simply skip"
      [ 2; 14; 1 ] [ 5; 18 ] Other;
    double_test
      "When you have a bigger double deck than the other person's card, you \
       can continue with that card"
      [ 1; 14; 27 ] [ 0; 13 ]
      (Continue [ 1; 14 ]);
    double_test
      "When you have a bigger double deck than the other person's card and \
       scramble up, you can continue with that card"
      [ 14; 28; 1 ] [ 0; 13 ]
      (Continue [ 14; 1 ]);
  ]

(** [triple_test name card1 card2 expected_output] ensures the correctness of
    the double card that can be put down by the player *)
let triple_test (name : string) (card1 : int list) (card2 : int list)
    (expected_output : choice) : test =
  name >:: fun _ -> assert_equal (triple card1 card2) expected_output

let triple_tests =
  [
    triple_test
      "When you dont have enough card, ie. no card, you may simply skip" []
      [ 0; 13; 26 ] Other;
    triple_test
      "When you dont have enough card, ie. one card, you may simply skip" [ 10 ]
      [ 0; 13; 26 ] Other;
    triple_test
      "When you dont have enough card, ie. two card, you may simply skip"
      [ 10; 20 ] [ 0; 13; 26 ] Other;
    triple_test
      "When you do have enough card, but not a greater deck than the other \
       play, you simply skip"
      [ 0; 13; 26 ] [ 1; 14; 27 ] Other;
    triple_test
      "When you have a bigger double deck than the other person's card, you \
       can continue with that card"
      [ 1; 14; 27 ] [ 0; 13; 26 ]
      (Continue [ 1; 14; 27 ]);
    triple_test
      "When you have a bigger double deck than the other person's card and \
       scramble up, you can continue with that card"
      [ 0; 12; 27; 14; 1; 10 ] [ 0; 13; 26 ]
      (Continue [ 27; 14; 1 ]);
  ]

(** [quad_test name card expected_ouput] asserts the correctness and quality of
    the function [quad lst] in comparison*)
let find_quad_test (name : string) (card : int list) (expected_output : choice)
    : test =
  name >:: fun _ -> assert_equal (find_quad card) expected_output

let find_quad_tests =
  [
    find_quad_test
      "When you have no card, you can't have a quad combination, so you can \
       only skip"
      [] Other;
    find_quad_test
      "When you have one card, you can't have a quad combination, so you can \
       only skip"
      [ 0 ] Other;
    find_quad_test
      "When you have two card, you can't have a quad combination, so you can \
       only skip"
      [ 10; 20 ] Other;
    find_quad_test
      "When you have three card, you can't have a quad combination, so you can \
       only skip"
      [ 0; 13; 26 ] Other;
    find_quad_test
      "You can continue if you posess any forms of quad card in your deck. In \
       this case [0;13;26;39] is the four 3s in poker"
      [ 0; 13; 26; 39 ]
      (Continue [ 0; 13; 26; 39 ]);
    find_quad_test
      "You can continue if you posess any forms of quad card in your deck. In \
       this case [1;14;27;40] is the four 4s in poker"
      [ 1; 14; 27; 40 ]
      (Continue [ 1; 14; 27; 40 ]);
    find_quad_test
      "You can continue if you posess any forms of quad card in your deck. In \
       this case [1;14;27;40] is the four 4s in poker"
      [ 1; 27; 28; 7; 2; 14; 4; 40 ]
      (Continue [ 1; 27; 14; 40 ]);
    find_quad_test
      "You can continue if you posess any forms of quad card in your deck. In \
       this case [1;14;27;40] is the four 4s in poker"
      [ 7; 12; 1; 3; 48; 9; 53; 17; 14; 6; 40; 2; 27 ]
      (Continue [ 1; 14; 40; 27 ]);
  ]

let comparison_tests =
  [
    "test suite for compare_card"
    >::: List.flatten
           [
             compare_card_tests;
             single_tests;
             double_tests;
             triple_tests;
             find_quad_tests;
           ];
  ]

(** [string_of_int_list lst] is the printer functions that print out the result
    of the update_ai_card_test function that helps with debugging *)
let string_of_int_list lst =
  let rec string_of_int_list lst =
    match lst with
    | [] -> ""
    | hd :: tl -> string_of_int hd ^ " " ^ string_of_int_list tl
  in
  "[" ^ string_of_int_list lst ^ "]"

(** [update_ai_card_test name cuurent_cards output_cards expected_output]
    asserts the quality and the correctness of the
    function[update_ai_cards current_card output_cards]*)
let update_ai_card_test (name : string) (current_cards : int list)
    (output_cards : int list) (expected_output : int list) : test =
  name >:: fun _ ->
  assert_equal
    (update_ai_cards current_cards output_cards)
    expected_output ~printer:string_of_int_list

let update_ai_card_tests =
  [
    update_ai_card_test
      "Removing any cards from a set of empty deck should return the empty \
       deck since there are no cards to draw from"
      [] [ 1 ] [];
    update_ai_card_test
      "Removing one card from a list of a card should return the empty set if \
       the card matches the output card"
      [ 1 ] [ 1 ] [];
    update_ai_card_test
      "Removing cards that are not in the list should return the original set \
       of card"
      [ 1 ] [ 2 ] [ 1 ];
    update_ai_card_test
      "Removing no card should return the original deck of card" [ 1 ] [] [ 1 ];
    update_ai_card_test
      "Removing a card from a deck of three card should remain two card, ie. \
       remove two from a list of [1;2;3] should output [1;3]"
      [ 1; 2; 3 ] [ 2 ] [ 1; 3 ];
    update_ai_card_test
      "Removing a card from a deck of three card should remain two card, ie. \
       remove two from a list of [1;2;3;7;8] should output [1;3;7;8]"
      [ 1; 2; 3; 7; 8 ] [ 2 ] [ 1; 3; 7; 8 ];
    update_ai_card_test
      "Removing two cards output order should also preverseves the output \
       where the two elements are removed"
      [ 5; 8; 1; 3; 20 ] [ 8; 1 ] [ 5; 3; 20 ];
  ]

(** [update_card_test name current_cards output_cards expected_output] ensures
    the quality and the correctness of the function
    [update_cards current_cards output_cards] *)
let update_card_test (name : string) (current_cards : int list)
    (output_cards : int list) (expected_output : int list) : test =
  name >:: fun _ ->
  assert_equal
    (update_cards current_cards output_cards)
    expected_output ~printer:string_of_int_list

let update_card_tests =
  [
    update_card_test
      "Removeing no card from an empty deck of card is the empty deck of card"
      [] [] [];
    update_card_test
      "Removeing any index of card from an empty set of deck should be the \
       empty deck"
      [] [ 5; 10; 20 ] [];
    update_card_test
      "Removeing a card from a deck of one cards at the index of 0" [ 1 ] [ 0 ]
      [];
    update_card_test
      "Removeing a card from a deck of one cards at the index output bound \
       should return the original deck"
      [ 1 ] [ 1 ] [ 1 ];
    update_card_test
      "Removeing a card from a deck of one cards at many indicies output bound \
       should return the original deck"
      [ 1 ] [ 1; 5; 6 ] [ 1 ];
    update_card_test
      "Removeing no card from a deck of three cards should return the original \
       deck"
      [ 1; 2; 3 ] [] [ 1; 2; 3 ];
    update_card_test
      "Removeing a card from a deck of three cards at the index of 2"
      [ 1; 2; 3 ] [ 2 ] [ 1; 2 ];
    update_card_test
      "Removeing a card from a deck of five cards at the index of 2"
      [ 1; 2; 3; 7; 8 ] [ 2 ] [ 1; 2; 7; 8 ];
  ]

(** [index_to_num_test name current_cards output_cards expected_output] ensures
    the quality and the correctness of the function
    [index_to_num current_cards output_cards]*)
let index_to_num_test (name : string) (current_cards : int list)
    (output_cards : int list) (expected_output : int list) : test =
  name >:: fun _ ->
  assert_equal
    (index_to_num current_cards output_cards)
    expected_output ~printer:string_of_int_list

let index_to_num_tests =
  [
    index_to_num_test
      "Extracting a new deck from an empty deck at no given indicies should \
       return the empty list"
      [] [] [];
    index_to_num_test
      "Extracting a new deck from an old deck at no given indicies should \
       return the empty list"
      [ 1; 2; 3 ] [] [];
    index_to_num_test
      "Extracting a new deck from a old deck at index of 2 should return a \
       singleton list of the element at index of 2"
      [ 1; 2; 3 ] [ 2 ] [ 3 ];
    index_to_num_test
      "Extracting a new deck from a old deck at index of 1 and 3 should return \
       a  list of the element at index of 1 and 3"
      [ 1; 2; 3; 7; 8 ] [ 1; 3 ] [ 2; 7 ];
  ]

let play_tests =
  [
    "test suite for play"
    >::: List.flatten
           [ update_ai_card_tests; update_card_tests; index_to_num_tests ];
  ]

(** [single_test name card1 card2 expected_output] ensures the correctness of
    the single card that can be put down by the player *)
let straight_test (name : string) (card : int list) expected_output : test =
  name >:: fun _ -> assert_equal (find_straight_list card) expected_output

let straight_tests =
  [
    straight_test "straight_test 1"
      [ 0; 1; 2; 3; 4; 5 + 13; 6 ]
      [ [ 0; 1; 2; 3; 4 ]; [ 1; 2; 3; 4; 5 + 13 ]; [ 2; 3; 4; 5 + 13; 6 ] ];
  ]

(**[single_test name card1 card2 expected_output] ensures the correctness of the
   single card that can be put down by the player *)
let two_test (name : string) (card : int list) expected_output : test =
  name >:: fun _ -> assert_equal (find_two_list card) expected_output

let two_tests =
  [
    two_test "two_test 1"
      [ 0; 1; 2; 3; 4; 5 + 13; 5 + 26; 4 + 13 ]
      [ [ 4; 4 + 13 ]; [ 5 + 13; 5 + 26 ] ];
    two_test "two_test 2" [ 0; 1; 2; 3; 4; 5 + 13 ] [];
  ]

(** [single_test name card1 card2 expected_output] ensures the correctness of
    the single card that can be put down by the player *)
let three_test (name : string) (card : int list) expected_output : test =
  name >:: fun _ -> assert_equal (find_three_list card) expected_output

let three_tests =
  [
    three_test "three_test 1"
      [ 0; 13; 26; 39; 1; 7; 14; 27; 40 ]
      [ [ 0; 13; 26 ]; [ 1; 14; 27 ] ];
    three_test "three_test 2" [ 0; 1; 2; 3; 4; 5 + 13 ] [];
  ]

let four_test (name : string) (card : int list) expected_output : test =
  name >:: fun _ -> assert_equal (find_four_list card) expected_output

let four_tests =
  [
    four_test "four_test 1"
      [ 0; 13; 26; 39; 1; 7; 14; 27; 40 ]
      [ [ 0; 13; 26; 39 ]; [ 1; 14; 27; 40 ] ];
    four_test "four_test 2" [ 0; 1; 2; 3; 4; 5 + 13 ] [];
  ]

let string_of_int_list_list_list lst =
  lst
  |> List.map (fun ll ->
         ll
         |> List.map (fun l ->
                l |> List.map string_of_int |> String.concat "; ")
         |> String.concat " | ")
  |> String.concat " || "
  |> Printf.sprintf "[[ [%s] ]]"

(** [split_test name card expected_outputs] ensures the correctness of the
    split_cards function *)
let split_test (name : string) (card : int list) expected_output : test =
  name >:: fun _ ->
  assert_equal (split_cards card) expected_output
    ~printer:string_of_int_list_list_list

(** [split_tests] handle a list of split_test *)
let split_tests =
  [
    split_test "A randome cards for testing split_cards"
      [
        2;
        2 + 13;
        4;
        4 + 13;
        5;
        6;
        6 + 13;
        6 + 26;
        7;
        8;
        8 + 13;
        9;
        10;
        10 + 13;
        11;
        12;
        12 + 13;
        12 + 26;
        52;
        53;
      ]
      [
        [ [ 52; 53 ] ];
        [];
        [ [ 5; 19; 7; 8; 9 ] ];
        [ [ 12; 12 + 13; 12 + 26 ] ];
        [ [ 2; 2 + 13 ]; [ 4; 4 + 13 ]; [ 6; 6 + 26 ]; [ 10; 10 + 13 ] ];
        [ [ 8 + 13 ]; [ 11 ] ];
      ];
    split_test "0;13;53 should be splitted in Pair 0;13 and Single 53"
      [ 0; 13; 53 ]
      [ []; []; []; []; [ [ 0; 13 ] ]; [ [ 53 ] ] ];
  ]

(** [s_test name card other expected_outputs] ensures the correctness of the
    straight function *)
let s_test (name : string) (card : int list) (other : int list) expected_output
    : test =
  name >:: fun _ -> assert_equal (straight card other) expected_output

(** [s_tests] handle a list of s_test *)
let s_tests =
  [
    s_test "There is no list greater than 0;1;2;3;4 in card"
      [ 0; 13; 26; 39; 1; 7; 14; 27; 40 ]
      [ 0; 1; 2; 3; 4 ] Other;
    s_test "1;2;3;4;5 is greater than 0;1;2;3;4 in card"
      [ 1; 2; 3; 4; 5; 7; 8; 9; 10 ]
      [ 0; 1; 2; 3; 4 ]
      (Continue [ 1; 2; 3; 4; 5 ]);
  ]

(** [type_test name cards expected_outputs] ensures the correctness of the
    getcardtype function *)
let type_test (name : string) (cards : int list) expected_output : test =
  name >:: fun _ -> assert_equal (getcardtype cards) expected_output

(** [type_tests] handle a list of type_test*)
let type_tests =
  [
    type_test "52 is Single" [ 52 ] Single;
    type_test "52;53 is Joker" [ 52; 53 ] Joker;
  ]

(** [valid_test name cards other expected_outputs] ensures the correctness of
    the check_valid function *)
let valid_test (name : string) (cards : int list) (other : int list)
    expected_output : test =
  name >:: fun _ -> assert_equal (check_valid cards other) expected_output

(** [valid_tests] handle a list of valid_test *)
let valid_tests =
  [
    valid_test "Joker Bomb is valid when previous players skip their turns"
      [ 53; 52 ] [] true;
  ]

(** [collab_test name cards expected_output] ensures the correctness of the
    single card that can be put down by the player *)
let collab_test (name : string) (cards : int list) expected_output : test =
  name >:: fun _ -> assert_equal (collab cards) expected_output

let collab_tests =
  [
    collab_test "collab_test single card greater than A" [ 10 ] true;
    collab_test "collab_test single card smaller than A" [ 1 ] false;
    collab_test "collab_test double card greater than K" [ 12; 25 ] true;
    collab_test "collab_test single card smaller than K" [ 3; 16 ] false;
    collab_test "collab_test triple card greater than J" [ 8; 21; 34 ] true;
    collab_test "collab_test triple card smaller than J" [ 3; 16; 29 ] false;
    collab_test "collab_test triple with single smaller than J" [ 3; 16; 29; 1 ]
      false;
    collab_test "collab_test triple with single smaller than J" [ 1; 3; 16; 29 ]
      false;
    collab_test "collab_test triple with single greater than J" [ 1; 8; 21; 34 ]
      true;
    collab_test "collab_test triple with single greater than J" [ 8; 21; 34; 2 ]
      true;
    collab_test "collab_test triple with double smaller than J"
      [ 3; 16; 29; 1; 14 ] false;
    collab_test "collab_test triple with double smaller than J"
      [ 1; 14; 3; 16; 29 ] false;
    collab_test "collab_test triple with double greater than J"
      [ 1; 14; 8; 21; 34 ] true;
    collab_test "collab_test triple with double greater than J"
      [ 8; 21; 34; 2; 15 ] true;
    collab_test "collab_test straight greater than [10; J; Q; K; A]"
      [ 7; 8; 9; 10; 11 ] true;
    collab_test "collab_test straight smaller than [10; J; Q; K; A]"
      [ 1; 2; 3; 4; 5 ] false;
    collab_test "collab_test quad greater than J" [ 8; 21; 34; 47 ] true;
    collab_test "collab_test quad smaller than J" [ 2; 15; 28; 41 ] false;
  ]

(** Let's run tests! *)
let suite =
  "test suite for Poker"
  >::: List.flatten
         [
           assign_card_tests;
           comparison_tests;
           play_tests;
           straight_tests;
           two_tests;
           three_tests;
           four_tests;
           split_tests;
           s_tests;
           type_tests;
           valid_tests;
           collab_tests;
         ]

let _ = run_test_tt_main suite
