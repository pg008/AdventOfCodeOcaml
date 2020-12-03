module List = struct
  include List

  let int_list l = List.map int_of_string l

  let sum l =
    let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
    sum' 0 l

  let lines = String.split_on_char '\n'

  let with_index l = List.mapi (fun i x -> (i, x)) l

  let count a l = l |> List.filter (fun x -> x = a) |> List.length

  let list_of_string s = s |> String.to_seq |> List.of_seq

  let count_filter f l = l |> List.filter f |> List.length

  let count_filter2 f l =
    List.fold_right (fun x s -> match f x with true -> s + 1 | false -> s) l 0
end
