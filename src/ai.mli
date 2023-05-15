val find_two_list : int list -> int list list
(**[find_two_list cards] returns empty list if cards doesn't have Pair, and
   returns a list of all Pair in cards otherwise*)

val find_three_list : int list -> int list list
(**[find_three_list cards] returns empty list if cards doesn't have Triple, and
   returns a list of all Triple in cards otherwise*)

val find_four_list : int list -> int list list
(**[find_four_list cards] returns empty list if cards doesn't have Quad, and
   returns a list of all Quad in cards otherwise*)

val find_single_list : int list -> int list list
(**[find_single_list cards] returns empty list if cards doesn't have Single, and
   returns a list of all Single in cards otherwise*)

val find_straight_list : int list -> int list list
(**[find_straight_list cards] returns empty list if cards doesn't have Straight,
   and returns a list of all Straight in cards otherwise*)

val find_jokers : int list -> int list list
(**[find_jokers cards] returns empty list if cards doesn't have Joker Bomb, and
   returns Joker Bomb otherwise*)

val make_fst_choice : int list -> Comparison.choice
(**[make_fst_choice cards] is the card that the ai will put down if the user
   skips and they are free to put down any patterns*)

val split_cards : int list -> int list list list
(**[split_cards] cards split cards into 6 group:
   [\[jokers\];\[quad\];\[straight\];\[triple\];\[double\];\[single\]], note
   [quad] is a list of int list with all choices for quad*)

val collab : int list -> bool
(** [collab prev_cards] returns true if previous cards are large according to
    the check functions defined above and false otherwise *)

val play : int list -> int list -> int list -> Comparison.choice
(** [play this other] returns AI's decision based on its current cards and the
    previous card *)
