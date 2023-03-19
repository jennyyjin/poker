type card = 
  Spade of int | Heart of int | Diamond of int | Club of int
let card_list = List.init 54 (fun i -> i + 1);;

let scrambled_list = List.sort (fun _ _ -> Random.int 3 - 1) card_list;;


let rec split_in_half lst =
  match lst with
  | [] -> [], []
  | [x] -> [x], []
  | x :: y :: rest ->
      let first_half, second_half = split_in_half rest in
      x :: first_half, y :: second_half

let sorted_deck = function
| (x , y) -> (List.sort x, List.sort y)

