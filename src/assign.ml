let card_list () = List.init 54 (fun i -> i)

let random n =
  Random.self_init ();
  Random.int n

let scrambled_list () = List.sort (fun _ _ -> random 3 - 1) (card_list ())

let rec split_in_three lst =
  match lst with
  | [] -> ([], [], [])
  | [ x ] -> ([ x ], [], [])
  | [ x; y ] -> ([ x ], [ y ], [])
  | [ x; y; z ] -> ([], [], [ x; y; z ])
  | x :: y :: z :: rest ->
      let first, second, third = split_in_three rest in
      (x :: first, y :: second, z :: third)
