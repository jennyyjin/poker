open OUnit2
open Poker
open Draw
open Comparison
open Assign
open Play

(**[split_in_half_test] name card_list expected_ouput asserts the corectness
   output of split_in_half *)
let split_in_half_test (name : string) (card_list : 'a list)
    (expected_output : 'a list * 'a list) : test =
  name >:: fun _ -> assert_equal (split_in_half card_list) expected_output

(**[card_list1] is a list of integers from 0 to 53 inclusive that use to
   represent the 54 cards in poker *)
let card_list1 = List.init 54 (fun i -> i)

(**[result1_right] is a list of even integers from 0 to 53 inclusive that use to
   represent half of the card taken by user 1 *)
let result1_right = List.init 27 (fun i -> (i * 2) + 1)

(**[result1_left] is a list of odd integers from 0 to 53 inclusive that use to
   represent half of the card taken by user 2 *)
let result1_left = List.init 27 (fun i -> i * 2)

let split_in_half_tests =
  [
    split_in_half_test "First test for split cards in half" card_list1
      (result1_left, result1_right)
    (* split_in_half_test "Second test for split cards in half" 4 14 1;; *);
  ]

let assign_card_tests =
  [ "test suite for adventure" >::: List.flatten [ split_in_half_tests ] ]

(**Test Methods in comparison.ml*)
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

let comparison_tests =
  [ "test suite for adventure" >::: List.flatten [ compare_card_tests ] ]

(**Test Methods in play.ml*)
let update_ai_card_test (name : string) (current_cards : int list)
    (output_cards : int list) (expected_output : int list) : test =
  name >:: fun _ ->
  assert_equal (update_ai_cards current_cards output_cards) expected_output

let update_ai_card_tests =
  [
    update_ai_card_test "A" [ 1; 2; 3 ] [ 2 ] [ 1; 3 ];
    update_ai_card_test "A" [ 1; 2; 3; 7; 8 ] [ 2 ] [ 1; 3; 7; 8 ];
  ]

let update_card_test (name : string) (current_cards : int list)
    (output_cards : int list) (expected_output : int list) : test =
  name >:: fun _ ->
  assert_equal (update_cards current_cards output_cards) expected_output

let update_card_tests =
  [
    update_card_test "A" [ 1; 2; 3 ] [ 2 ] [ 1; 2 ];
    update_card_test "A" [ 1; 2; 3; 7; 8 ] [ 2 ] [ 1; 2; 7; 8 ];
  ]

let index_to_num_test (name : string) (current_cards : int list)
    (output_cards : int list) (expected_output : int list) : test =
  name >:: fun _ ->
  assert_equal (index_to_num current_cards output_cards) expected_output

let index_to_num_tests =
  [
    index_to_num_test "A" [ 1; 2; 3 ] [ 2 ] [ 3 ];
    index_to_num_test "A" [ 1; 2; 3; 7; 8 ] [ 1; 3 ] [ 2; 7 ];
  ]

let play_tests =
  [
    "test suite for adventure"
    >::: List.flatten
           [ update_ai_card_tests; update_card_tests; index_to_num_tests ];
  ]

(**Let's run tests!*)
let suite =
  "test suite for Poker"
  >::: List.flatten [ assign_card_tests; comparison_tests; play_tests ]

let _ = run_test_tt_main suite
