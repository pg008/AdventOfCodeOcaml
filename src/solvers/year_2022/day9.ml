open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  module VecSet = Set.Make (struct
    type t = int * int

    let compare = compare
  end)

  let parse_move move =
    let parse_direction = function
      | "R" -> (1, 0)
      | "L" -> (-1, 0)
      | "U" -> (0, 1)
      | "D" -> (0, -1)
      | _ -> failwith "Invalid direction"
    in
    match String.split_on_char ' ' move with
    | [ dir; amnt ] ->
        let direction = parse_direction dir in
        List.init (int_of_string amnt) (fun _ -> direction)
    | _ -> failwith "Invalid move"

  let ( +: ) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

  let ( -: ) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

  let double x = (x, x)

  let update_tail tail head =
    let x, y = head -: tail in
    if abs x <= 1 && abs y <= 1 then tail
    else (x / max (abs x) 1, y / max (abs y) 1) +: tail

  let calculate_tail_positions head_positions =
    List.fold_left_map
      (fun x y -> update_tail x y |> double)
      (0, 0) head_positions
    |> snd
    |> List.cons (0, 0)

  let rec apply_n n f acc = if n <= 0 then acc else apply_n (n - 1) f (f acc)

  let naloga1 data =
    let lines = List.lines data in
    let head_positions =
      lines |> List.map parse_move |> List.flatten
      |> List.fold_left_map (fun x y -> x +: y |> double) (0, 0)
      |> snd
      |> List.cons (0, 0)
    in
    calculate_tail_positions head_positions
    |> VecSet.of_list |> VecSet.elements |> List.length |> string_of_int

  let naloga2 data _part1 =
    let lines = List.lines data in
    let head_positions =
      lines |> List.map parse_move |> List.flatten
      |> List.fold_left_map (fun x y -> x +: y |> double) (0, 0)
      |> snd
      |> List.cons (0, 0)
    in
    apply_n 9 calculate_tail_positions head_positions
    |> VecSet.of_list |> VecSet.elements |> List.length |> string_of_int
end
