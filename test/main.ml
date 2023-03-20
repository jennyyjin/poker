open OUnit2
open Poker
open Draw
open Comparison

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

let suite = "test suite for Poker" >::: List.flatten [ comparison_tests ]
let _ = run_test_tt_main suite
