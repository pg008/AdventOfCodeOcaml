let bin_to_decimal l =
  let rec bin_to_s a = function
    | [] -> a
    | x :: xs -> if x then bin_to_s ((2 * a) + 1) xs else bin_to_s (2 * a) xs
  in
  bin_to_s 0 l
