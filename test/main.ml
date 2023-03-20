open OUnit2
open Poker
open Draw
open Comparison
open Assign

(**Test Methods in assign_card.ml*)
let split_in_half_test (name : string) (card_list : 'a list)
    (expected_output : 'a list * 'a list) : test =
  name >:: fun _ -> assert_equal (split_in_half card_list) expected_output

let card_list1 =
  [
    1;
    2;
    3;
    4;
    5;
    6;
    7;
    8;
    9;
    10;
    11;
    12;
    13;
    14;
    15;
    16;
    17;
    18;
    19;
    20;
    21;
    22;
    23;
    24;
    25;
    26;
    27;
    28;
    29;
    30;
    31;
    32;
    33;
    34;
    35;
    36;
    37;
    38;
    39;
    40;
    41;
    42;
    43;
    44;
    45;
    46;
    47;
    48;
    49;
    50;
    51;
    52;
    53;
    54;
  ]

let result1_left =
  [
    1;
    3;
    5;
    7;
    9;
    11;
    13;
    15;
    17;
    19;
    21;
    23;
    25;
    27;
    29;
    31;
    33;
    35;
    37;
    39;
    41;
    43;
    45;
    47;
    49;
    51;
    53;
  ]

let result1_right =
  [
    2;
    4;
    6;
    8;
    10;
    12;
    14;
    16;
    18;
    20;
    22;
    24;
    26;
    28;
    30;
    32;
    34;
    36;
    38;
    40;
    42;
    44;
    46;
    48;
    50;
    52;
    54;
  ]

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
  name >:: fun _ -> assert_equal (compare_card card1 card2) expected_output

let compare_card_tests =
  [
    compare_card_test "compare_card 1 2 is -1" 1 2 (-1);
    compare_card_test "compare_card 4 14 is 1" 4 14 1;
  ]

let comparison_tests =
  [ "test suite for adventure" >::: List.flatten [ compare_card_tests ] ]

(**Let's run tests!*)
let suite =
  "test suite for Poker"
  >::: List.flatten [ comparison_tests; assign_card_tests ]

let _ = run_test_tt_main suite
