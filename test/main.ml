open OUnit2
open Poker
open Draw
open Comparison
open Assign
open Play

(**[string_of_int_pair lst1 lst2] is the printer functions that print out the
   result of the split_in_half function that helps with debugging *)
let string_of_int_pair (lst1, lst2) =
  let rec string_of_int_list lst =
    match lst with
    | [] -> ""
    | hd :: tl -> string_of_int hd ^ " " ^ string_of_int_list tl
  in
  "(" ^ string_of_int_list lst1 ^ ", " ^ string_of_int_list lst2 ^ ")"

(**[split_in_half_test] name card_list expected_ouput asserts the corectness
   output of split_in_half *)
let split_in_half_test (name : string) (card_list : 'a list)
    (expected_output : 'a list * 'a list) : test =
  name >:: fun _ ->
  assert_equal (split_in_half card_list) expected_output
    ~printer:string_of_int_pair

(**[card_list1] is a list of integers from 0 to 53 inclusive that use to
   represent the 54 cards in poker *)
let card_lst = List.init 54 (fun i -> i)

(**[card_fst] is a list of even integers from 0 to 53 inclusive that use to
   represent half of the card taken by user 1 *)
let card_fst = List.init 27 (fun i -> i * 2)

(**[card_snd] is a list of odd integers from 0 to 53 inclusive that use to
   represent half of the card taken by user 2 *)
let card_snd = List.init 27 (fun i -> (i * 2) + 1)

let split_in_half_tests =
  [
    split_in_half_test
      "Split no card should not have cards for the first person nor the second \
       person "
      [] ([], []);
    split_in_half_test
      "Split one card should give the fst the first card and snd the second no \
       card "
      [ 0 ] ([ 0 ], []);
    split_in_half_test
      "Split two cards should give the fst the first card and snd the second \
       card "
      [ 0; 1 ] ([ 0 ], [ 1 ]);
    split_in_half_test
      "Split two cards should give the fst the first card and snd the second \
       card if the order has been change"
      [ 1; 0 ] ([ 1 ], [ 0 ]);
    split_in_half_test "First test for split cards in half " card_lst
      (card_fst, card_snd);
  ]

let assign_card_tests =
  [ "test suite for split card" >::: List.flatten [ split_in_half_tests ] ]

(**[compare_card_test name card1 card2 expected_outputs] ensures the correctness
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
  ]

let double_test (name : string) (card1 : int list) (card2 : int list)
    (expected_output : choice) : test =
  name >:: fun _ -> assert_equal (double card1 card2) expected_output

let double_tests = []

let triple_test (name : string) (card1 : int list) (card2 : int list)
    (expected_output : choice) : test =
  name >:: fun _ -> assert_equal (triple card1 card2) expected_output

let triple_tests = []

let quad_test (name : string) (card1 : int list) (expected_output : choice) :
    test =
  name >:: fun _ -> assert_equal (quad card1) expected_output

let quad_tests = []

let comparison_tests =
  [
    "test suite for compare_card"
    >::: List.flatten [ compare_card_tests; single_tests ];
  ]

(**[string_of_int_list lst] is the printer functions that print out the result
   of the update_ai_card_test function that helps with debugging *)
let string_of_int_list lst =
  let rec string_of_int_list lst =
    match lst with
    | [] -> ""
    | hd :: tl -> string_of_int hd ^ " " ^ string_of_int_list tl
  in
  "[" ^ string_of_int_list lst ^ "]"

(**[update_ai_card_test name cuurent_cards output_cards expected_output] asserts
   the quality and the correctness of the
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

(**[update_card_test name current_cards output_cards expected_output] ensures
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

(**Let's run tests!*)
let suite =
  "test suite for Poker"
  >::: List.flatten [ assign_card_tests; comparison_tests; play_tests ]

let _ = run_test_tt_main suite
