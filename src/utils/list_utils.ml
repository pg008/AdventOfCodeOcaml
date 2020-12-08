module List = struct
  include Stdlib.List

  let int_list l = List.map int_of_string l

  let sum l =
    let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
    sum' 0 l

  let lines = String.split_on_char '\n'

  let rec make_patches rtr acc = function
    | [] -> List.rev (acc :: rtr)
    | "" :: (_ :: _ as rest) -> make_patches (acc :: rtr) "" rest
    | l :: rest -> make_patches rtr (acc ^ " " ^ l) rest

  let groups s = make_patches [] "" s

  let with_index l = List.mapi (fun i x -> (i, x)) l

  let count a l = l |> List.filter (fun x -> x = a) |> List.length

  let list_of_string s = s |> String.to_seq |> List.of_seq

  let count_filter f l = l |> List.filter f |> List.length

  let count_filter2 f l =
    List.fold_right (fun x s -> match f x with true -> s + 1 | false -> s) l 0

  let maximum l = List.fold_left max (List.nth l 0) l

  let rec reduce fn = function
    | [] -> failwith "Empty list"
    | [ a ] -> a
    | x :: xs -> fn x (reduce fn xs)

  let reduce2 fn l =
    let rec reduce' acc fn l =
      match l with x :: xs -> reduce' (fn x acc) fn xs | [] -> acc
    in
    match l with [] -> failwith "Empty list" | x :: xs -> reduce' x fn xs
end
